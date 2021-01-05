module Arkham.Types.Location.Cards.AlchemyLabs
  ( alchemyLabs
  , AlchemyLabs(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AlchemyLabs = AlchemyLabs Attrs
  deriving newtype (Show, ToJSON, FromJSON)

alchemyLabs :: AlchemyLabs
alchemyLabs = AlchemyLabs $ baseAttrs
  "02057"
  (LocationName "Alchemy Labs" Nothing)
  EncounterSet.ExtracurricularActivity
  5
  (Static 0)
  Squiggle
  [Hourglass]
  [Miskatonic]

instance HasModifiersFor env AlchemyLabs where
  getModifiersFor _ target (AlchemyLabs attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AlchemyLabs where
  getActions iid NonFast (AlchemyLabs attrs@Attrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
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
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (Investigate iid (locationId attrs) source SkillIntellect False)
    SuccessfulInvestigation iid _ source | isSource attrs source -> do
      maid <- fmap unStoryAssetId <$> getId (CardCode "02059")
      l <$ case maid of
        Just aid -> unshiftMessage (TakeControlOfAsset iid aid)
        Nothing -> pure ()
    _ -> AlchemyLabs <$> runMessage msg attrs
