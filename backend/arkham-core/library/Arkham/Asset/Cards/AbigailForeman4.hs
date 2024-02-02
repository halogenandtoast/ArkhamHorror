module Arkham.Asset.Cards.AbigailForeman4 (
  abigailForeman4,
  AbigailForeman4 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype AbigailForeman4 = AbigailForeman4 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

abigailForeman4 :: AssetCard AbigailForeman4
abigailForeman4 = ally AbigailForeman4 Cards.abigailForeman4 (1, 2)

instance HasModifiersFor AbigailForeman4 where
  getModifiersFor (AssetTarget aid) (AbigailForeman4 a) | aid /= toId a = do
    controller <- field AssetController (toId a)
    case controller of
      Nothing -> pure []
      Just iid -> do
        placement <- field AssetPlacement aid
        pure case placement of
          AttachedToAsset aid' _ | aid' == toId a -> toModifiers a [AsIfUnderControlOf iid]
          _ -> []
  getModifiersFor _ _ = pure []

instance HasAbilities AbigailForeman4 where
  getAbilities (AbigailForeman4 a) =
    [ controlledAbility a 1 criteria $ FastAbility Free
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          ( ActivateAbility #after You
              $ PerformableAbility [IgnoreAllCosts]
              <> AbilityIsActionAbility
              <> AssetAbility (AssetAttachedToAsset (AssetWithId $ toId a))
          )
          (exhaust a)
    ]
   where
    criteria = case a.controller of
      Just iid -> exists (#tome <> AssetWithPlacement (InPlayArea iid))
      Nothing -> Never

getAbility :: [Window] -> (Ability, [Window])
getAbility [] = error "No windows"
getAbility ((windowType -> Window.ActivateAbility _ ws ab) : _) = (ab, ws)
getAbility (_ : rest) = getAbility rest

instance RunMessage AbigailForeman4 where
  runMessage msg a@(AbigailForeman4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      as <- selectList $ assetControlledBy iid <> #tome <> AssetWithPlacement (InPlayArea iid)
      mAttachedAsset <- selectOne $ AssetAttachedToAsset $ AssetWithId $ toId attrs
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ targetLabel a'
            $ [PlaceAsset a' $ AttachedToAsset (toId attrs) (Just $ InPlayArea iid)]
            <> [PlaceAsset a'' (InPlayArea iid) | a'' <- toList mAttachedAsset]
            <> [RefillSlots iid]
          | a' <- as
          ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getAbility -> (ab, ws)) _ -> do
      push $ UseAbility iid ab ws
      pure a
    _ -> AbigailForeman4 <$> runMessage msg attrs
