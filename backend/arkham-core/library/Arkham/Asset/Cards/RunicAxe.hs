module Arkham.Asset.Cards.RunicAxe (runicAxe, RunicAxe (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Classes.HasGame
import Arkham.DamageEffect qualified as Msg
import Arkham.Discover
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed, withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest.Target
import Arkham.Investigator.Types (Field (InvestigatorLocation))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DiscoverClues, EnemyDefeated)
import Arkham.Modifier
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

override :: AssetAttrs -> InvestigatorId -> CriteriaOverride
override a iid =
  CriteriaOverride
    $ EnemyCriteria
    $ ThisEnemy
    $ EnemyWithoutModifier CannotBeAttacked
    <> oneOf
      [ EnemyAt (ConnectedFrom $ locationWithInvestigator iid)
      , enemyAtLocationWith iid <> AloofEnemy <> CanEngageEnemy (a.ability 1)
      , enemyAtLocationWith iid <> not_ AloofEnemy
      ]

instance HasModifiersFor RunicAxe where
  getModifiersFor target (RunicAxe (With a _)) | isTarget a target = do
    modified a
      $ guard (a `hasCustomization` Heirloom)
      *> [ReduceCostOf (CardWithId a.cardId) 1, AddTrait Relic]
  getModifiersFor (AbilityTarget iid ab) (RunicAxe (With a _)) | ab.source == toSource a = do
    modified
      a
      [ CanModify $ EnemyFightActionCriteria $ override a iid
      | a.use Charge > 0 && a `hasCustomization` InscriptionOfTheHunt
      ]
  getModifiersFor _ _ = pure []

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
      let replenish = if attrs `hasCustomization` Saga then 2 else 1
      pure . RunicAxe . (`with` meta) $ attrs & tokensL . ix Charge %~ \n -> if n < 4 then n + replenish else n
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (SkillModifier #combat 1)
      if attrs `hasCustomization` InscriptionOfTheHunt && attrs.use Charge > 0
        then chooseFightEnemyMatch iid (attrs.ability 1) $ CanFightEnemyWithOverride (override attrs iid)
        else chooseFightEnemy iid (attrs.ability 1)
      pure a
    ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) eid -> do
      when (attrs.use Charge > 0) do
        needsHunt <-
          not
            <$> (eid <=~> (enemyAtLocationWith iid <> oneOf [AloofEnemy <> enemyEngagedWith iid, not_ AloofEnemy]))
        let imbueAgain = if attrs `hasCustomization` Saga then [Do msg, msg] else [msg]
        if needsHunt && attrs `hasCustomization` InscriptionOfTheHunt
          then pushAll [SpendUses (attrs.ability 1) (toTarget attrs) Charge 1, DoStep (fromEnum Hunt) msg]
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
    Do msg'@(ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) _) -> do
      choices <- availableInscriptions iid attrs meta
      chooseOne iid
        $ Label "Do not use additional imbue from Saga " []
        : [ Label (tshow i) [DoStep (fromEnum i) msg']
          | i <- choices
          ]
      pure a
    DoStep n (ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) eid) -> do
      let inscription = toEnum n
      send $ "Imbued Runic Axe with " <> tshow inscription
      case inscription of
        Accuracy -> skillTestModifier (attrs.ability 1) iid (SkillModifier #combat 2)
        Power -> skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
        Glory -> pure ()
        Elders -> pure ()
        Hunt -> do
          mLoc <- field InvestigatorLocation iid
          field EnemyLocation eid >>= traverse_ \loc -> do
            if Just loc /= mLoc
              then push $ Move $ move (attrs.ability 1) iid loc
              else do
                engaged <- eid <=~> enemyEngagedWith iid
                unless engaged $ push $ EngageEnemy iid eid Nothing False
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
                enemies <- select $ enemyEngagedWith iid <> not_ (EnemyWithId eid)
                for_ enemies \eid' -> push $ EnemyDamage eid' $ Msg.delayDamage $ Msg.attack (attrs.ability 1) furyCount
                pushAll $ Msg.checkDefeated attrs <$> enemies
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
