module Arkham.Investigator.Cards.CharlieKane (charlieKane, CharlieKane (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestMatchingSkillIcons, withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection
import Arkham.Slot

newtype CharlieKane = CharlieKane InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

charlieKane :: InvestigatorCard CharlieKane
charlieKane =
  investigator CharlieKane Cards.charlieKane
    $ Stats {health = 6, sanity = 6, willpower = 1, intellect = 1, combat = 1, agility = 1}

instance HasAbilities CharlieKane where
  getAbilities (CharlieKane a) =
    [ restrictedAbility a 1 (Self <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility (ExhaustAssetCost (#ally <> assetControlledBy a.id))
    ]

instance HasChaosTokenValue CharlieKane where
  getChaosTokenValue iid ElderSign (CharlieKane attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 3)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CharlieKane where
  runMessage msg i@(CharlieKane attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      pushAll $ replicate 3 $ AddSlot iid AllySlot (Slot (toSource iid) [])
      CharlieKane <$> liftRunMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 _ p -> do
      icons <- getSkillTestMatchingSkillIcons
      n <- case p.exhausted of
        [AssetTarget aid] -> fieldMap AssetCard (count (`elem` icons) . (.skills)) aid
        _ -> error "Unhandled"
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue (1 + n))
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      allies <-
        select
          $ AssetExhausted
          <> AssetAt (locationWithInvestigator iid)
          <> oneOf [AssetControlledBy (affectsOthers Anyone), not_ (AssetControlledBy Anyone)]
      when (notNull allies) do
        chooseOne iid [targetLabel ally [Ready (toTarget ally)] | ally <- allies]
      pure i
    _ -> CharlieKane <$> liftRunMessage msg attrs
