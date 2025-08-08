module Arkham.Event.Events.PowerWord (powerWord, PowerWord (..)) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.DamageEffect
import Arkham.Discover
import Arkham.Enemy.Types (
  Field (EnemyEvade, EnemyFight, EnemyHealth, EnemyHealthDamage, EnemyLocation, EnemySanityDamage),
 )
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Customization
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher hiding (DiscoverClues, EnemyEvaded)
import Arkham.Projection
import Arkham.Taboo
import Data.Map.Strict qualified as Map

data Command = GoCommand | CowerCommand | BetrayCommand | MercyCommand | ConfessCommand | DistractCommand
  deriving stock (Show, Eq, Enum, Bounded, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype PowerWord = PowerWord EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- The taboo is weird here because Arkham DB isn't going to handle the different xp, it will just have the xp in mercy

commandLabel :: Command -> Text
commandLabel = \case
  GoCommand -> "Go"
  CowerCommand -> "Cower"
  BetrayCommand -> "Betray"
  MercyCommand -> "Mercy"
  ConfessCommand -> "Confess"
  DistractCommand -> "Distract"

toKey :: Command -> Key
toKey = \case
  GoCommand -> "go"
  CowerCommand -> "cower"
  BetrayCommand -> "betray"
  MercyCommand -> "mercy"
  ConfessCommand -> "confess"
  DistractCommand -> "distract"

powerWord :: EventCard PowerWord
powerWord = event PowerWord Cards.powerWord

-- if we have thrice spoken then we want ANY PowerWord that can issue the command, otherwise just us
instance HasAbilities PowerWord where
  getAbilities (PowerWord a) = case a.placement of
    AttachedToEnemy eid ->
      let
        commands = availableCommands a
        additionalEnemyCriteria = oneOf $ map (\(_, f) -> f eid) commands
        fromLocation =
          if a `hasCustomization` Bonded
            then oneOf [YourLocation, ConnectedFrom YourLocation]
            else YourLocation
        handleThriceSpoken inner =
          if a `hasCustomization` ThriceSpoken
            then
              oneOf
                [ exists $ inner <> additionalEnemyCriteria
                , exists inner
                    <> exists
                      ( eventIs Cards.powerWord
                          <> not_ (EventWithId a.id)
                          <> oneOf (EventWithMetaKey <$> map (toKey . fst) commands)
                      )
                ]
            else exists $ inner <> additionalEnemyCriteria
       in
        [ controlledAbility
            a
            1
            (handleThriceSpoken $ EnemyWithId eid <> EnemyAt fromLocation <> CanParleyEnemy You)
            parleyAction_
        | notNull commands || tabooed TabooList21 a -- the taboo'd version will initiate a skill test
        ]
          <> [ restrictedAbility a 2 ControlsThis $ FastAbility Free
             | a `hasCustomization` GreaterControl
             ]
    _ -> []

availableCommands :: EventAttrs -> [(Command, EnemyId -> EnemyMatcher)]
availableCommands a = filter (\(x, _) -> getMetaKey (toKey x) a) (allCommands a)

allCommands :: EventAttrs -> [(Command, EnemyId -> EnemyMatcher)]
allCommands a =
  ( GoCommand
  , \eid -> EnemyWhenLocation $ ConnectedTo (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
  )
    : [ (CowerCommand, \_ -> ReadyEnemy)
      | not (tabooed TabooList21 a) || (tabooed TabooList21 a && a `hasCustomization` Mercy)
      ]
      <> [ ( BetrayCommand
           , \eid ->
               EnemyAt (locationWithEnemy eid)
                 <> EnemyCanBeDamagedBySource (a.ability 1)
                 <> EnemyWithMaybeFieldLessThanOrEqualToThis eid EnemyFight
           )
         | a `hasCustomization` Betray
         ]
      <> [ ( MercyCommand
           , \eid ->
               EnemyWhenInvestigator
                 $ oneOf
                   [ HealableInvestigator (a.ability 1) #damage
                       $ InvestigatorAt
                       $ LocationWithEnemy
                       $ EnemyWithId eid
                       <> EnemyWithNonZeroField EnemyHealthDamage
                   , HealableInvestigator (a.ability 1) #horror
                       $ InvestigatorAt
                       $ LocationWithEnemy
                       $ EnemyWithId eid
                       <> EnemyWithNonZeroField EnemySanityDamage
                   ]
           )
         | a `hasCustomization` Mercy && not (tabooed TabooList21 a)
         ]
      <> [ ( ConfessCommand
           , \eid ->
               EnemyWhenLocation
                 $ locationWithEnemy eid
                 <> LocationWithShroudLessThanOrEqualToLessThanEnemyMaybeField eid EnemyHealth
                 <> LocationWithDiscoverableCluesBy You
           )
         | a `hasCustomization` Confess
         ]
      <> [ ( DistractCommand
           , \eid ->
               EnemyAt (locationWithEnemy eid)
                 <> EnemyCanBeEvadedBy (a.ability 1)
                 <> EnemyWithMaybeFieldLessThanOrEqualToThis eid EnemyEvade
           )
         | a `hasCustomization` Distract
         ]

determineMeta :: HasGame m => EventAttrs -> m Value
determineMeta attrs = do
  let used = getMetaKeyDefault "used" [] attrs
  case attrs.placement of
    AttachedToEnemy eid -> do
      commandMap <-
        Map.fromList <$> for (allCommands attrs) \(command, f) -> (command,) <$> (eid <=~> f eid)
      let isValid cmd = cmd `notElem` used && Map.findWithDefault False cmd commandMap
      pure
        $ object
          [ toKey GoCommand .= isValid GoCommand
          , toKey CowerCommand .= isValid CowerCommand
          , toKey BetrayCommand .= isValid BetrayCommand
          , toKey MercyCommand .= isValid MercyCommand
          , toKey ConfessCommand .= isValid ConfessCommand
          , toKey DistractCommand .= isValid DistractCommand
          , "used" .= used
          ]
    _ -> pure $ object ["used" .= used]

runAbility :: ReverseQueue m => InvestigatorId -> EventAttrs -> Bool -> m ()
runAbility iid attrs canTonguetwister = do
  maps <- for (availableCommands attrs) \(command, f) -> do
    powerWords <-
      if attrs `hasCustomization` ThriceSpoken
        then select $ eventIs Cards.powerWord <> EventWithMetaKey (toKey command)
        else pure [attrs.id | getMetaKey (toKey command) attrs]

    let isViable eid' = eid' <=~> f eid'

    powerWordsWithEnemies <-
      concatMap (\(k, es) -> (k,) <$> es) <$> forToSnd powerWords \word ->
        filterM isViable =<< select (EnemyWithAttachedEvent $ EventWithId word)

    pure
      $ if null powerWordsWithEnemies
        then mempty
        else singletonMap command powerWordsWithEnemies

  let allMap = Map.unionsWith (<>) maps
  let tonguetwister =
        guard (canTonguetwister && attrs `hasCustomization` Tonguetwister)
          $> Do (UseCardAbility iid (toSource attrs) 1 [] NoPayment)
  choices <- forToSnd (Map.toList allMap) $ \(cmd, pairings) -> evalQueueT do
    chooseOrRunOneAtATime
      iid
      [ targetLabel eid [DoStep (fromEnum cmd) (UseCardAbility iid (toSource pid) 1 [] NoPayment)]
      | (pid, eid) <- pairings
      ]

  when (notNull choices) do
    chooseOne iid
      $ [ Label "Do not use second command (Tonguetwister)" []
        | not canTonguetwister && attrs `hasCustomization` Tonguetwister
        ]
      <> [Label (commandLabel cmd) (msgs <> tonguetwister) | ((cmd, _), msgs) <- choices]

instance RunMessage PowerWord where
  runMessage msg e@(PowerWord attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy
      chooseOne
        iid
        [targetLabel enemy [PlaceEvent attrs.id $ AttachedToEnemy enemy] | enemy <- enemies]
      meta' <- determineMeta attrs
      pure . PowerWord $ attrs & metaL .~ meta'
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      if tabooed TabooList21 attrs
        then do
          case attrs.placement of
            AttachedToEnemy eid -> do
              sid <- getRandom
              parley sid iid (attrs.ability 1) eid #willpower
                $ Fixed (if tabooed TabooList24 attrs then 2 else 3)
            _ -> error "invalid call"
        else runAbility iid attrs True
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      runAbility iid attrs True
      pure e
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      runAbility iid attrs False
      pure e
    DoStep step (UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.placement of
        AttachedToEnemy eid -> do
          let command = toEnum step
          case command of
            GoCommand -> do
              choices <- select $ ConnectedFrom (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
              when (notNull choices) $ chooseOrRunOne iid $ targetLabels choices (only . EnemyMove eid)
            CowerCommand -> pushWhenM (eid <=~> ReadyEnemy) $ Exhaust (toTarget eid)
            BetrayCommand -> do
              enemies <-
                select
                  $ EnemyAt (locationWithEnemy eid)
                  <> EnemyCanBeDamagedBySource (attrs.ability 1)
                  <> EnemyWithMaybeFieldLessThanOrEqualToThis eid EnemyFight
              when (notNull enemies) do
                chooseOrRunOne
                  iid
                  [ targetLabel enemy [EnemyDamage enemy $ nonAttack (Just iid) (attrs.ability 1) 1] | enemy <- enemies
                  ]
            MercyCommand -> do
              let source = attrs.ability 1
              damage <- field EnemyHealthDamage eid
              horror <- field EnemySanityDamage eid
              horrorInvestigators <-
                if horror > 0 then select (HealableInvestigator source #horror $ colocatedWith iid) else pure []
              damageInvestigators <-
                if damage > 0 then select (HealableInvestigator source #damage $ colocatedWith iid) else pure []
              choices <- forToSnd (nub $ horrorInvestigators <> damageInvestigators) $ \investigator -> evalQueueT do
                chooseOrRunOne iid
                  $ [ Label ("Heal " <> tshow damage <> " damage") [HealDamage (toTarget investigator) source damage]
                    | investigator `elem` damageInvestigators
                    ]
                  <> [ Label ("Heal " <> tshow horror <> " horror") [HealHorror (toTarget investigator) source horror]
                     | investigator `elem` horrorInvestigators
                     ]

              when (notNull choices) $ chooseOrRunOne iid $ map (uncurry targetLabel) choices
            ConfessCommand ->
              field EnemyLocation eid >>= traverse_ \lid ->
                pushWhenM (lid <=~> LocationWithDiscoverableCluesBy (InvestigatorWithId iid))
                  $ DiscoverClues iid
                  $ discover lid (attrs.ability 1) 1
            DistractCommand -> do
              enemies <-
                select
                  $ EnemyAt (locationWithEnemy eid)
                  <> EnemyCanBeEvadedBy (attrs.ability 1)
                  <> EnemyWithMaybeFieldLessThanOrEqualToThis eid EnemyEvade
              when (notNull enemies)
                $ chooseOrRunOne iid
                $ targetLabels enemies (only . EnemyEvaded iid)
          let used = getMetaKeyDefault "used" [] attrs
          meta' <- determineMeta (setMetaKey @[Command] "used" (command : used) attrs)
          pure . PowerWord $ attrs & metaL .~ meta'
        _ -> error "Invalid"
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ ReturnToHand iid (toTarget attrs)
      pure e
    EndRound -> do
      meta' <- determineMeta (setMetaKey @[Command] "used" [] attrs)
      pure . PowerWord $ attrs & metaL .~ meta'
    _ -> do
      meta' <- determineMeta attrs
      PowerWord <$> liftRunMessage msg (attrs & metaL .~ meta')
