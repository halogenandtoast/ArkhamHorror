module Arkham.Location.Cards.BrackishWaters (
  BrackishWaters (..),
  brackishWaters,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype BrackishWaters = BrackishWaters LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brackishWaters :: LocationCard BrackishWaters
brackishWaters = location BrackishWaters Cards.brackishWaters 1 (Static 0)

instance HasModifiersFor BrackishWaters where
  getModifiersFor (InvestigatorTarget iid) (BrackishWaters attrs) =
    pure $
      toModifiers
        attrs
        [ CannotPlay (CardWithType AssetType)
        | iid `elem` locationInvestigators attrs
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities BrackishWaters where
  getAbilities (BrackishWaters attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
        attrs
        1
        (Here <> Negate (AssetExists $ assetIs Assets.fishingNet))
        $ ActionAbility Nothing
        $ Costs
          [ ActionCost 1
          , DiscardFromCost
              2
              (FromHandOf You <> FromPlayAreaOf You)
              (CardWithType AssetType)
          ]
      | locationRevealed attrs
      ]

instance RunMessage BrackishWaters where
  runMessage msg l@(BrackishWaters attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l
            <$ push
              (beginSkillTest iid source (toTarget attrs) SkillAgility 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          fishingNet <- PlayerCard <$> genPlayerCard Assets.fishingNet
          l <$ push (TakeControlOfSetAsideAsset iid fishingNet)
    _ -> BrackishWaters <$> runMessage msg attrs
