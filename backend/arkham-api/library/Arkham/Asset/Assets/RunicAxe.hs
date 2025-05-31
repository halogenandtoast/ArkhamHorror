module Arkham.Asset.Assets.RunicAxe (runicAxe) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Classes.HasGame
import Arkham.DamageEffect qualified as Msg
import Arkham.Discover
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Location (getAccessibleLocations, getLocationOf, withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest.Target
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DiscoverClues, EnemyDefeated)
import Arkham.Movement
import Arkham.Projection
import Arkham.Trait (Trait (Relic))

data Inscription = Accuracy | Power | Glory | Elders | Hunt | Fury
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

newtype Metadata = Metadata {inscriptions :: [Inscription]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RunicAxe = RunicAxe (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runicAxe :: AssetCard RunicAxe
runicAxe = asset (RunicAxe . (`with` Metadata [])) Cards.runicAxe

override :: AssetAttrs -> InvestigatorId -> Int -> CriteriaOverride
override a iid distance =
  CriteriaOverride
    $ EnemyCriteria
    $ ThisEnemy
    $ EnemyWithoutModifier CannotBeAttacked
    <> oneOf
      [ EnemyWhenInvestigator (InvestigatorWithId iid <> InvestigatorWithoutModifier CannotMove)
          <> ( if a.use Charge > 1
                 then
                   oneOf
                     [ not_ AloofEnemy <> atDistance distance
                     , EnemyIsEngagedWith Anyone <> atDistance distance
                     , CanEngageEnemyWithOverride (CriteriaOverride $ EnemyCriteria $ ThisEnemy AnyEnemy)
                         <> atDistance (distance - 1)
                     ]
                 else oneOf [not_ AloofEnemy, EnemyIsEngagedWith Anyone]
             )
      , enemyAtLocationWith iid
          <> AloofEnemy
          <> oneOf [EnemyIsEngagedWith Anyone, CanEngageEnemy (a.ability 1)]
      , enemyAtLocationWith iid <> not_ AloofEnemy
      ]
 where
  atDistance n =
    EnemyAt
      $ if n == 1
        then AccessibleFrom (locationWithInvestigator iid)
        else LocationWithAccessiblePath (toSource a) n (InvestigatorWithId iid) Anywhere

instance HasModifiersFor RunicAxe where
  getModifiersFor (RunicAxe (With a _)) = do
    modifiedWhen_
      a
      (a `hasCustomization` Heirloom)
      (CardIdTarget a.cardId)
      [ReduceCostOf (CardWithId a.cardId) 1, AddTrait Relic]
    for_ a.controller \iid -> do
      modifySelfWhen a (a `hasCustomization` Heirloom) [AddTrait Relic]
      void $ runMaybeT do
        guard (a.use Charge > 0 && a `hasCustomization` InscriptionOfTheHunt)
        ab <- MaybeT $ selectOne (AbilityIs (toSource a) 1)
        let hasPower = a `hasCustomization` AncientPower
        let distance = if hasPower then min 3 (a.use Charge) else 1
        modified_
          a
          (AbilityTarget iid ab.ref)
          [CanModify $ EnemyFightActionCriteria $ override a iid distance]

instance HasAbilities RunicAxe where
  getAbilities (RunicAxe (With a _)) = [restrictedAbility a 1 ControlsThis fightAction_]

availableInscriptions :: HasGame m => InvestigatorId -> AssetAttrs -> Metadata -> m [Inscription]
availableInscriptions iid attrs meta = do
  connectedLocations <- notNull <$> getAccessibleLocations iid (attrs.ability 1)
  unengagedEnemies <- selectAny $ CanEngageEnemy (attrs.ability 1) <> enemyAtLocationWith iid
  let
    allInscriptions =
      Accuracy
        : Power
        : [Glory | attrs `hasCustomization` InscriptionOfGlory]
          <> [Elders | attrs `hasCustomization` InscriptionOfTheElders]
          <> [ Hunt
             | attrs `hasCustomization` InscriptionOfTheHunt && (connectedLocations || unengagedEnemies)
             ]
          <> [Fury | attrs `hasCustomization` InscriptionOfFury]
  let allowedTimes = if attrs `hasCustomization` AncientPower then 3 else 1
  pure $ filter (\i -> count (== i) (inscriptions meta) < allowedTimes) allInscriptions

instance RunMessage RunicAxe where
  runMessage msg a@(RunicAxe (With attrs meta)) = runQueueT $ case msg of
    Do BeginRound -> do
      let amount = if attrs `hasCustomization` Saga then 2 else 1
      pure . RunicAxe . (`with` meta) $ attrs & tokensL %~ replenishN Charge 4 amount
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
      if attrs `hasCustomization` InscriptionOfTheHunt && attrs.use Charge > 0
        then do
          let hasPower = attrs `hasCustomization` AncientPower
          let distance = if hasPower then min 3 (attrs.use Charge) else 1
          chooseFightEnemyMatch sid iid (attrs.ability 1)
            $ CanFightEnemyWithOverride (override attrs iid distance)
        else chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    ChoseEnemy _sid iid (isAbilitySource attrs 1 -> True) eid -> do
      when (attrs.use Charge > 0) do
        needsHunt <-
          andM
            [ not
                <$> ( eid
                        <=~> (enemyAtLocationWith iid <> oneOf [AloofEnemy <> EnemyIsEngagedWith Anyone, not_ AloofEnemy])
                    )
            , not <$> (coerce eid <=~> locationWithInvestigator iid)
            ]
        let imbueAgain = if attrs `hasCustomization` Scriptweaver then [Do msg, msg] else [msg]
        if needsHunt && attrs `hasCustomization` InscriptionOfTheHunt
          then
            pushAll
              $ [SpendUses (attrs.ability 1) (toTarget attrs) Charge 1, DoStep (fromEnum Hunt) msg]
              <> imbueAgain
          else do
            choices <- availableInscriptions iid attrs meta
            chooseOne iid
              $ Label "Do not spend charges" []
              : [ Label (tshow i)
                    $ [SpendUses (attrs.ability 1) (toTarget attrs) Charge 1, DoStep (fromEnum i) msg]
                    <> imbueAgain
                | i <- choices
                ]
      pure a
    Do msg'@(ChoseEnemy _sid iid (isAbilitySource attrs 1 -> True) _) -> do
      choices <- availableInscriptions iid attrs meta
      chooseOne iid
        $ Label "Do not use additional imbue from Scriptweaver " []
        : [ Label (tshow i) [DoStep (fromEnum i) msg']
          | i <- choices
          ]
      pure a
    DoStep n (ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) eid) -> do
      let inscription = toEnum n
      send $ "Imbued Runic Axe with " <> tshow inscription
      case inscription of
        Accuracy -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
        Power -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
        Glory -> pure ()
        Elders -> pure ()
        Hunt -> do
          mLoc <- getLocationOf iid
          isLocation <- coerce eid <=~> Anywhere
          if isLocation
            then push $ Move $ move (attrs.ability 1) iid (coerce eid)
            else
              getLocationOf eid >>= traverse_ \loc -> do
                if Just loc /= mLoc
                  then do
                    for_ mLoc \loc' -> do
                      accessibleLocations <- getAccessibleLocations iid (attrs.ability 1)
                      closestLocationIds <- select $ ClosestPathLocation loc' loc
                      let locations = filter (`elem` closestLocationIds) accessibleLocations
                      chooseOne iid $ targetLabels locations (only . Move . move (attrs.ability 1) iid)
                  else do
                    engaged <- eid <=~> enemyEngagedWith iid
                    unless engaged $ enemyEngageInvestigator eid iid
        Fury -> pure ()
      RunicAxe . (`with` Metadata (inscription : inscriptions meta)) <$> liftRunMessage msg attrs
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      withLocationOf iid \loc -> do
        let elderCount = count (== Elders) (inscriptions meta)
        field LocationShroud loc >>= traverse_ \shroud -> do
          pushWhen (elderCount > 0 && n >= shroud)
            $ DiscoverClues iid
            $ discover loc (attrs.ability 1) elderCount

      let furyCount = count (== Fury) (inscriptions meta)
      when (furyCount > 0) do
        getSkillTestTarget
          >>= traverse_ \target -> do
            case target of
              EnemyTarget eid -> do
                enemies <- select $ EnemyIsEngagedWith Anyone <> not_ (EnemyWithId eid)
                for_ enemies \eid' -> push $ EnemyDamage eid' $ Msg.delayDamage $ Msg.attack (attrs.ability 1) furyCount
                pushAll $ Msg.checkDefeated (attrs.ability 1) <$> enemies
              _ -> pure ()

      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      push $ DoStep (count (== Glory) (inscriptions meta)) msg
      pure a
    DoStep n msg'@(EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _) | n > 0 -> do
      for_ attrs.controller \iid -> do
        mCanDraw <- Msg.drawCardsIfCan iid (attrs.ability 1) 1
        canHealDamage <- canHaveDamageHealed (attrs.ability 1) iid
        canHealHorror <- canHaveHorrorHealed (attrs.ability 1) iid
        when (isJust mCanDraw || canHealDamage || canHealHorror) do
          chooseOne iid
            $ [Label "Draw 1 card" [drawing] | drawing <- maybeToList mCanDraw]
            <> [Label "Heal 1 damage" [HealDamage (toTarget iid) (attrs.ability 1) 1] | canHealDamage]
            <> [Label "Heal 1 horror" [HealHorror (toTarget iid) (attrs.ability 1) 1] | canHealHorror]
          push $ DoStep (n - 1) msg'
      pure a
    ResolvedAbility ab | ab.source == toSource attrs -> do
      RunicAxe . (`with` Metadata []) <$> liftRunMessage msg attrs
    _ -> RunicAxe . (`with` meta) <$> liftRunMessage msg attrs
