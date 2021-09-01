module Arkham.Types.Location.Cards.BrackishWaters
  ( BrackishWaters(..)
  , brackishWaters
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype BrackishWaters = BrackishWaters LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brackishWaters :: LocationCard BrackishWaters
brackishWaters = location
  BrackishWaters
  Cards.brackishWaters
  1
  (Static 0)
  Triangle
  [Squiggle, Square, Diamond, Hourglass]

instance HasModifiersFor env BrackishWaters where
  getModifiersFor _ (InvestigatorTarget iid) (BrackishWaters attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [(AssetType, mempty)]
      | iid `elem` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env BrackishWaters where
  getAbilities iid window (BrackishWaters attrs) =
    withBaseAbilities iid window attrs $ do
      pure
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

instance LocationRunner env => RunMessage env BrackishWaters where
  runMessage msg l@(BrackishWaters attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        fishingNet <- PlayerCard <$> genPlayerCard Assets.fishingNet
        l <$ push (TakeControlOfSetAsideAsset iid fishingNet)
    _ -> BrackishWaters <$> runMessage msg attrs
