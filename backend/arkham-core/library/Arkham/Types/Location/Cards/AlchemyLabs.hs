module Arkham.Types.Location.Cards.AlchemyLabs
  ( alchemyLabs
  , AlchemyLabs(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (alchemyLabs)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Window

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationId -> AlchemyLabs
alchemyLabs = AlchemyLabs . baseAttrs
  Cards.alchemyLabs
  5
  (Static 0)
  Squiggle
  [Hourglass]

instance HasModifiersFor env AlchemyLabs where
  getModifiersFor _ target (AlchemyLabs attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AlchemyLabs where
  getActions iid NonFast (AlchemyLabs attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      let
        ability = mkAbility
          (toSource attrs)
          1
          (ActionAbility (Just Action.Investigate) (ActionCost 1))
      pure
        [ ActivateCardAbilityAction iid ability
        | iid `elem` locationInvestigators
        ]
  getActions iid window (AlchemyLabs attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env AlchemyLabs where
  runMessage msg l@(AlchemyLabs attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (Investigate iid (locationId attrs) source SkillIntellect False)
    SuccessfulInvestigation iid _ source | isSource attrs source -> do
      maid <- fmap unStoryAssetId <$> getId (CardCode "02059")
      l <$ case maid of
        Just aid -> unshiftMessage (TakeControlOfAsset iid aid)
        Nothing -> pure ()
    _ -> AlchemyLabs <$> runMessage msg attrs
