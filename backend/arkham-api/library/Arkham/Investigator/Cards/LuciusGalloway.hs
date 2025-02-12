module Arkham.Investigator.Cards.LuciusGalloway (luciusGalloway) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype LuciusGalloway = LuciusGalloway InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

luciusGalloway :: InvestigatorCard LuciusGalloway
luciusGalloway =
  investigator LuciusGalloway Cards.luciusGalloway
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 4, combat = 1, agility = 5}

instance HasAbilities LuciusGalloway where
  getAbilities (LuciusGalloway a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> AbleToDiscoverCluesAt YourLocation)
        $ freeReaction (SkillTestResult #after You #evading (SuccessResult $ atLeast 2))
    ]

instance HasChaosTokenValue LuciusGalloway where
  getChaosTokenValue iid ElderSign (LuciusGalloway attrs) | iid == toId attrs = do
    pure $ elderSignValue $ CountEnemies $ at_ (locationWithInvestigator iid)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage LuciusGalloway where
  runMessage msg i@(LuciusGalloway attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure i
    _ -> LuciusGalloway <$> liftRunMessage msg attrs
