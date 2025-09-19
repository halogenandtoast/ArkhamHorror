module Arkham.Location.Cards.SubterraneanSwamp (subterraneanSwamp) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SubterraneanSwamp = SubterraneanSwamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subterraneanSwamp :: LocationCard SubterraneanSwamp
subterraneanSwamp = location SubterraneanSwamp Cards.subterraneanSwamp 2 (PerPlayer 2)

instance HasAbilities SubterraneanSwamp where
  getAbilities (SubterraneanSwamp a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted
            a
            1
            (Here <> thisExists a LocationWithoutClues <> youExist (InvestigatorWithSupply Canteen))
          $ FastAbility Free
      , restricted a 2 (youExist $ not_ $ InvestigatorWithSupply Pocketknife)
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      ]

instance RunMessage SubterraneanSwamp where
  runMessage msg l@(SubterraneanSwamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pickSupply iid StickyGoop
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      setActions iid (attrs.ability 2) 0
      endYourTurn iid
      pure l
    _ -> SubterraneanSwamp <$> liftRunMessage msg attrs
