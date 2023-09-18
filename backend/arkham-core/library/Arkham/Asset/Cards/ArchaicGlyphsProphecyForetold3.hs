module Arkham.Asset.Cards.ArchaicGlyphsProphecyForetold3 (
  archaicGlyphsProphecyForetold3,
  ArchaicGlyphsProphecyForetold3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Projection

newtype ArchaicGlyphsProphecyForetold3 = ArchaicGlyphsProphecyForetold3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsProphecyForetold3 where
  getAbilities (ArchaicGlyphsProphecyForetold3 a) =
    [investigateAbility a 1 (Costs [ActionCost 1, assetUseCost a Charge 1]) ControlsThis]

archaicGlyphsProphecyForetold3 :: AssetCard ArchaicGlyphsProphecyForetold3
archaicGlyphsProphecyForetold3 =
  asset ArchaicGlyphsProphecyForetold3 Cards.archaicGlyphsProphecyForetold3

instance RunMessage ArchaicGlyphsProphecyForetold3 where
  runMessage msg a@(ArchaicGlyphsProphecyForetold3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        skillType <- field LocationInvestigateSkill lid
        push $ Investigate iid lid (toSource attrs) (Just $ toTarget attrs) skillType False
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ (isTarget attrs -> True) _ -> do
      enemies <- selectList EnemyEngagedWithYou
      pushAll
        $ InvestigatorDiscoverClues iid lid (toAbilitySource attrs 1) 1 (Just Action.Investigate)
          : [ chooseOne iid
              $ Label "No evasion" [] : [targetLabel enemy [EnemyEvaded iid enemy] | enemy <- enemies]
            | notNull enemies
            ]
      pure a
    _ -> ArchaicGlyphsProphecyForetold3 <$> runMessage msg attrs
