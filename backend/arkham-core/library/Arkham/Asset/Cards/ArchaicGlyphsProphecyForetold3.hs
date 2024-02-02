module Arkham.Asset.Cards.ArchaicGlyphsProphecyForetold3 (
  archaicGlyphsProphecyForetold3,
  ArchaicGlyphsProphecyForetold3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Investigate
import Arkham.Matcher hiding (EnemyEvaded)

newtype ArchaicGlyphsProphecyForetold3 = ArchaicGlyphsProphecyForetold3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

instance HasAbilities ArchaicGlyphsProphecyForetold3 where
  getAbilities (ArchaicGlyphsProphecyForetold3 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

archaicGlyphsProphecyForetold3 :: AssetCard ArchaicGlyphsProphecyForetold3
archaicGlyphsProphecyForetold3 = asset ArchaicGlyphsProphecyForetold3 Cards.archaicGlyphsProphecyForetold3

instance RunMessage ArchaicGlyphsProphecyForetold3 where
  runMessage msg a@(ArchaicGlyphsProphecyForetold3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ (isTarget attrs -> True) _ -> do
      enemies <- selectList $ enemyEngagedWith iid
      player <- getPlayer iid
      pushAll
        $ toMessage (viaInvestigate $ discover iid lid (toAbilitySource attrs 1) 1)
        : [ chooseOne player $ Label "No evasion" [] : targetLabels enemies (only . EnemyEvaded iid)
          | notNull enemies
          ]
      pure a
    _ -> ArchaicGlyphsProphecyForetold3 <$> runMessage msg attrs
