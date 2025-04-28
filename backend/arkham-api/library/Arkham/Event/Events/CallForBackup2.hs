module Arkham.Event.Events.CallForBackup2 (callForBackup2) where

import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Damage
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (canDiscoverCluesAtYourLocation)
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype CallForBackup2 = CallForBackup2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callForBackup2 :: EventCard CallForBackup2
callForBackup2 = event CallForBackup2 Cards.callForBackup2

control :: HasGame m => [ClassSymbol] -> ClassSymbol -> m Bool
control ks k =
  if k `notElem` ks
    then selectAny (ControlledBy You <> basic (CardWithClass k))
    else pure False

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc = HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

instance RunMessage CallForBackup2 where
  runMessage msg e@(CallForBackup2 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      do_ msg
      pure e
    Do msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      let chosen = toResultDefault [] attrs.meta

      hasRogue <-
        andM
          [ control chosen Rogue
          , notNull <$> getAccessibleLocations iid attrs
          ]

      hasGuardian <-
        andM
          [ control chosen Guardian
          , iid <=~> InvestigatorWithoutModifier CannotDealDamage
          , selectAny $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource (toSource attrs)
          ]

      hasSeeker <-
        andM
          [ control chosen Seeker
          , canDiscoverCluesAtYourLocation NotInvestigate iid
          ]

      hasMystic <-
        andM
          [ control chosen Mystic
          , orM
              [ selectAny (HealableInvestigator (toSource attrs) #horror $ colocatedWith iid)
              , selectAny (healableAsset attrs #horror $ locationWithInvestigator iid)
              ]
          ]

      hasSurvivor <-
        andM
          [ control chosen Survivor
          , orM
              [ selectAny (HealableInvestigator (toSource attrs) #damage $ colocatedWith iid)
              , selectAny (healableAsset attrs #damage $ locationWithInvestigator iid)
              ]
          ]

      when (hasRogue || hasGuardian || hasSeeker || hasMystic || hasSurvivor) do
        chooseOneM iid do
          when hasRogue do
            labeled "{rogue} card, you may move to a connecting location" do
              doStep 1 msg'
              do_ msg'
          when hasGuardian do
            labeled "{guardian} card, deal 1 damage to an enemy at your location" do
              doStep 2 msg'
              do_ msg'
          when hasSeeker do
            labeled "{seeker} card, discover 1 clue at your location" do
              doStep 3 msg'
              do_ msg'
          when hasMystic do
            labeled "{mystic} card, heal 1 horror from any card" do
              doStep 4 msg'
              do_ msg'
          when hasSurvivor do
            labeled "{survivor} card, heal 1 damage from any card" do
              doStep 5 msg'
              do_ msg'
          labeled "Done choosing options" nothing
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      locations <- getAccessibleLocations iid attrs
      chooseTargetM iid locations (moveTo attrs iid)
      let chosen = toResultDefault [] attrs.meta
      pure $ overAttrs (setMeta (Rogue : chosen)) e
    DoStep 2 (PlayThisEvent iid (is attrs -> True)) -> do
      enemies <- select $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) attrs 1
      let chosen = toResultDefault [] attrs.meta
      pure $ overAttrs (setMeta (Guardian : chosen)) e
    DoStep 3 (PlayThisEvent iid (is attrs -> True)) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      let chosen = toResultDefault [] attrs.meta
      pure $ overAttrs (setMeta (Seeker : chosen)) e
    DoStep 4 (PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select (HealableInvestigator (toSource attrs) #horror $ colocatedWith iid)
      assets <- select (healableAsset attrs #horror $ locationWithInvestigator iid)
      chooseOneM iid do
        for_ investigators \iid' -> horrorLabeled iid' $ healHorror iid' attrs 1
        for_ assets \aid -> assetHorrorLabeled aid $ healHorror aid attrs 1
      let chosen = toResultDefault [] attrs.meta
      pure $ overAttrs (setMeta (Mystic : chosen)) e
    DoStep 5 (PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select (HealableInvestigator (toSource attrs) #damage $ colocatedWith iid)
      assets <- select (healableAsset attrs #damage $ locationWithInvestigator iid)
      chooseOneM iid do
        for_ investigators \iid' -> damageLabeled iid' $ healDamage iid' attrs 1
        for_ assets \aid -> assetDamageLabeled aid $ healDamage aid attrs 1
      let chosen = toResultDefault [] attrs.meta
      pure $ overAttrs (setMeta (Survivor : chosen)) e
    _ -> CallForBackup2 <$> liftRunMessage msg attrs
