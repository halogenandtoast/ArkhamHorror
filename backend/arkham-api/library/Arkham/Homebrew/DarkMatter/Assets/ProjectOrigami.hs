module Arkham.Homebrew.DarkMatter.Assets.ProjectOrigami (projectOrigami) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

{- | One of Starfall's three escape objectives: an investigator at Mount Sinai must
control Universal Archives, which is then removed from the game and paid for with
2[per_investigator] clues as a group.
-}
newtype ProjectOrigami = ProjectOrigami AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

projectOrigami :: AssetCard ProjectOrigami
projectOrigami = asset ProjectOrigami Cards.projectOrigami

instance HasAbilities ProjectOrigami where
  getAbilities (ProjectOrigami a) =
    [ restricted
        a
        1
        ( exists
            $ assetIs Cards.universalArchives
            <> AssetControlledBy (InvestigatorAt $ locationIs Locations.mountSinai)
        )
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

instance RunMessage ProjectOrigami where
  runMessage msg a@(ProjectOrigami attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      push $ PlaceAsset attrs.id NextToAct
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (assetIs Cards.universalArchives) >>= traverse_ (push . RemoveFromGame . toTarget)
      addToVictory iid attrs
      pure a
    _ -> ProjectOrigami <$> liftRunMessage msg attrs
