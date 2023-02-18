module Arkham.Asset.Cards.ArchaicGlyphsProphecyForetold3
  ( archaicGlyphsProphecyForetold3
  , ArchaicGlyphsProphecyForetold3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Projection
import Arkham.Target

newtype ArchaicGlyphsProphecyForetold3 = ArchaicGlyphsProphecyForetold3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsProphecyForetold3 where
  getAbilities (ArchaicGlyphsProphecyForetold3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

archaicGlyphsProphecyForetold3 :: AssetCard ArchaicGlyphsProphecyForetold3
archaicGlyphsProphecyForetold3 =
  asset ArchaicGlyphsProphecyForetold3 Cards.archaicGlyphsProphecyForetold3

instance RunMessage ArchaicGlyphsProphecyForetold3 where
  runMessage msg a@(ArchaicGlyphsProphecyForetold3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      case mlid of
        Nothing -> push $ Discard (toSource attrs) (toTarget attrs)
        Just lid -> do
          skillType <- field LocationInvestigateSkill lid
          pushAll
            [ Investigate
              iid
              lid
              (toSource attrs)
              (Just $ toTarget attrs)
              skillType
              False
            , Discard (toSource attrs) (toTarget attrs)
            ]
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ target _
      | isTarget attrs target -> do
        enemies <- selectList EnemyEngagedWithYou
        pushAll
          $ InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate)
          : [ chooseOne
                iid
                $ Label "No evasion" []
                : [ targetLabel enemy [EnemyEvaded iid enemy]
                  | enemy <- enemies
                  ]
            | notNull enemies
            ]
        pure a
    _ -> ArchaicGlyphsProphecyForetold3 <$> runMessage msg attrs
