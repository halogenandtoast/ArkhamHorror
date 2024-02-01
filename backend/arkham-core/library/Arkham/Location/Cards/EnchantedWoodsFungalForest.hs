module Arkham.Location.Cards.EnchantedWoodsFungalForest (
  enchantedWoodsFungalForest,
  EnchantedWoodsFungalForest (..),
)
where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy

newtype EnchantedWoodsFungalForest = EnchantedWoodsFungalForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

enchantedWoodsFungalForest :: LocationCard EnchantedWoodsFungalForest
enchantedWoodsFungalForest = location EnchantedWoodsFungalForest Cards.enchantedWoodsFungalForest 5 (PerPlayer 1)

instance HasAbilities EnchantedWoodsFungalForest where
  getAbilities (EnchantedWoodsFungalForest attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ ForcedAbility $ TurnBegins #when You]

instance RunMessage EnchantedWoodsFungalForest where
  runMessage msg l@(EnchantedWoodsFungalForest attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RequestChaosTokens (toAbilitySource attrs 1) (Just iid) (Reveal 1) SetAside
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      player <- getPlayer iid
      when (any ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace) tokens) $ do
        pushAll [assignDamage iid (toAbilitySource attrs 1) 1, LoseActions iid (toAbilitySource attrs 1) 1]
      push $ chooseOne player [Label "Continue" [ResetChaosTokens (toAbilitySource attrs 1)]]
      pure l
    _ -> EnchantedWoodsFungalForest <$> runMessage msg attrs
