module Arkham.Homebrew.CircusExMortis.Treacheries.WildHysteria (wildHysteria) where

import Arkham.Ability
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.SkillType
import Arkham.Treachery.Import.Lifted

newtype WildHysteria = WildHysteria TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wildHysteria :: TreacheryCard WildHysteria
wildHysteria = treachery WildHysteria Cards.wildHysteria

instance HasModifiersFor WildHysteria where
  getModifiersFor (WildHysteria a) = case a.placement of
    AttachedToLocation lid ->
      modified_
        a
        lid
        [ AdditionalCostToEnter (ActionCost 1)
        , AdditionalCostToLeave (ActionCost 1)
        , AdditionalCostToInvestigate (ActionCost 1)
        ]
    _ -> pure mempty

instance HasAbilities WildHysteria where
  getAbilities (WildHysteria a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage WildHysteria where
  runMessage msg t@(WildHysteria attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getLocationOf iid >>= traverse_ (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      here <- select $ InvestigatorAt (locationWithInvestigator iid)
      moons <- sum <$> traverse (fmap length . getSealedMoonTokens) here
      chooseOneM iid do
        for_ allSkills \sType ->
          skillLabeled sType $ parley sid iid (attrs.ability 1) attrs sType (Fixed $ 3 + moons)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WildHysteria <$> liftRunMessage msg attrs
