module Arkham.Investigator.Cards.DarrellSimmons (darrellSimmons, DarrellSimmons (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token

newtype DarrellSimmons = DarrellSimmons InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darrellSimmons :: InvestigatorCard DarrellSimmons
darrellSimmons =
  startsWith [Assets.darrellsKodak]
    $ investigator DarrellSimmons Cards.darrellSimmons
    $ Stats {health = 6, sanity = 8, willpower = 2, intellect = 5, combat = 2, agility = 3}

instance HasAbilities DarrellSimmons where
  getAbilities (DarrellSimmons a) =
    [ playerLimit PerTestOrAbility
        $ restrictedAbility a 1 (Self <> DuringSkillTest SkillTestAtYourLocation)
        $ FastAbility (UseCost (AssetControlledBy You) Evidence 1)
    ]

instance HasChaosTokenValue DarrellSimmons where
  getChaosTokenValue iid ElderSign (DarrellSimmons attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage DarrellSimmons where
  runMessage msg i@(DarrellSimmons attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier iid SkillTestTarget (Difficulty (-2))
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      assets <- select $ assetControlledBy iid <> AssetCanHaveUses Evidence
      when (notNull assets) do
        chooseOne
          iid
          [targetLabel asset [PlaceTokens (toSource ElderSign) (toTarget asset) Evidence 1] | asset <- assets]
      pure i
    _ -> DarrellSimmons <$> liftRunMessage msg attrs
