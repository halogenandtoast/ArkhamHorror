module Arkham.Location.Cards.HubDimension (hubDimension) where

import Arkham.Ability
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (AncientOne, RitualSite))

newtype HubDimension = HubDimension LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hubDimension :: LocationCard HubDimension
hubDimension = location HubDimension Cards.hubDimension 6 (Static 0)

instance HasModifiersFor HubDimension where
  getModifiersFor (HubDimension a) = do
    modifySelfWhen a a.revealed [CannotHaveAttachments, ConnectedToWhen (be a) (LocationWithTrait RitualSite)]
    modifySelectWhen
      a
      a.revealed
      (LocationWithTrait RitualSite)
      [ConnectedToWhen (LocationWithTrait RitualSite) (be a)]

instance HasAbilities HubDimension where
  getAbilities (HubDimension a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage HubDimension where
  runMessage msg l@(HubDimension attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 2)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      let damage = if n >= 3 then 3 else 2
      clues <- getSpendableClueCount [iid]
      around <- getCluesAroundHubDimension
      ancientOnes <- select $ EnemyWithTrait AncientOne
      unless (null ancientOnes) do
        scenarioI18n $ chooseOrRunOneM iid do
          when (clues >= 3) do
            labeled' "spendClues" do
              spendClues iid 3
              chooseTargetM iid ancientOnes $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) damage
          when (around >= 2) do
            labeled' "removeCluesFromAroundHubDimension" do
              removeCluesFromAroundHubDimension 2
              chooseTargetM iid ancientOnes $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) damage
      pure l
    _ -> HubDimension <$> liftRunMessage msg attrs
