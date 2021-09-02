module Arkham.Types.Location.Cards.Parlor where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
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
import Arkham.Types.Source
import Arkham.Types.Target

newtype Parlor = Parlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = location Parlor Cards.parlor 2 (Static 0) Diamond [Square]

instance HasModifiersFor env Parlor where
  getModifiersFor _ target (Parlor attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Parlor where
  getAbilities (Parlor attrs) = withResignAction
    attrs
    [ restrictedAbility
        (ProxySource
          (AssetMatcherSource $ assetIs Cards.litaChantler)
          (toSource attrs)
        )
        1
        (Unowned <> OnSameLocation)
      $ ActionAbility (Just Action.Parley)
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env Parlor where
  runMessage msg l@(Parlor attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource attrs source && locationRevealed -> do
        selectOne (assetIs Cards.litaChantler) >>= \case
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
        selectOne (assetIs Cards.litaChantler) >>= \case
          Nothing -> error "this ability should not be able to be used"
          Just aid -> l <$ push (TakeControlOfAsset iid aid)
    _ -> Parlor <$> runMessage msg attrs
