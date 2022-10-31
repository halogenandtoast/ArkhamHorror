module Arkham.Location.Cards.Parlor where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype Parlor = Parlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = location Parlor Cards.parlor 2 (Static 0)

instance HasModifiersFor Parlor where
  getModifiersFor target (Parlor attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ = pure []

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

instance RunMessage Parlor where
  runMessage msg l@(Parlor attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid (isProxyAbility attrs 1 -> True) _ _
      | locationRevealed -> do
        selectOne (assetIs Cards.litaChantler) >>= \case
          Nothing -> error "this ability should not be able to be used"
          Just aid -> l <$ push
            (BeginSkillTest
              iid
              (toSource attrs)
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
