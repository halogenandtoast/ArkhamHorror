module Arkham.Location.Cards.ArkhamWoodsGreatWillow where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsGreatWillow )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillTest
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: LocationCard ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow =
  location ArkhamWoodsGreatWillow Cards.arkhamWoodsGreatWillow 4 (PerPlayer 1)

instance HasAbilities ArkhamWoodsGreatWillow where
  getAbilities (ArkhamWoodsGreatWillow attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here
          $ ForcedAbility
          $ SkillTestResult Timing.After You (SkillTestOnTreachery AnyTreachery)
          $ SuccessResult AnyValue
        | locationRevealed attrs
        ]

instance RunMessage ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      getSkillTestSource >>= \case
        Just (TreacherySource tid) -> do
          push $ CreateEffect
            (toCardCode attrs)
            Nothing
            source
            (TreacheryTarget tid)
          pure l
        _ -> error "Invalid use of Arkham Woods: Great Willow ability"
    _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
