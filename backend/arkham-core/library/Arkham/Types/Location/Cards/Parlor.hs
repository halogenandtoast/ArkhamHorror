module Arkham.Types.Location.Cards.Parlor where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (parlor)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype Parlor = Parlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = location Parlor Cards.parlor 2 (Static 0) Diamond [Square]

instance HasModifiersFor env Parlor where
  getModifiersFor _ target (Parlor attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Parlor where
  getActions iid NonFast (Parlor attrs@LocationAttrs {..}) | locationRevealed =
    do
      actions <- withResignAction iid NonFast attrs
      maid <- fmap unStoryAssetId <$> getId (CardCode "01117")
      case maid of
        Nothing -> pure actions
        Just aid -> do
          miid <- fmap unOwnerId <$> getId aid
          assetLocationId <- getId aid
          investigatorLocationId <- getId @LocationId iid
          pure
            $ actions
            <> [ UseAbility
                   iid
                   (mkAbility
                     (ProxySource (AssetSource aid) (toSource attrs))
                     1
                     (ActionAbility (Just Action.Parley) $ ActionCost 1)
                   )
               | isNothing miid
                 && Just investigatorLocationId
                 == assetLocationId
               ]
  getActions iid window (Parlor attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env Parlor where
  runMessage msg l@(Parlor attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource attrs source && locationRevealed -> do
        maid <- fmap unStoryAssetId <$> getId (CardCode "01117")
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> l <$ push
            (BeginSkillTest
              iid
              source
              (AssetTarget aid)
              (Just Action.Parley)
              SkillIntellect
              4
            )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        maid <- fmap unStoryAssetId <$> getId (CardCode "01117")
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> l <$ push (TakeControlOfAsset iid aid)
    _ -> Parlor <$> runMessage msg attrs
