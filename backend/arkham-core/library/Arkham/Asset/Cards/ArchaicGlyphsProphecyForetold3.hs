module Arkham.Asset.Cards.ArchaicGlyphsProphecyForetold3
  ( archaicGlyphsProphecyForetold3
  , ArchaicGlyphsProphecyForetold3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.SkillType
import Arkham.Target

newtype ArchaicGlyphsProphecyForetold3 = ArchaicGlyphsProphecyForetold3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsProphecyForetold3 where
  getAbilities (ArchaicGlyphsProphecyForetold3 a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Investigate)
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

archaicGlyphsProphecyForetold3 :: AssetCard ArchaicGlyphsProphecyForetold3
archaicGlyphsProphecyForetold3 =
  asset ArchaicGlyphsProphecyForetold3 Cards.archaicGlyphsProphecyForetold3

instance AssetRunner env => RunMessage ArchaicGlyphsProphecyForetold3 where
  runMessage msg a@(ArchaicGlyphsProphecyForetold3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      a <$ pushAll
        [ Investigate
          iid
          lid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillIntellect
          False
        , Discard (toTarget attrs)
        ]
    Successful (Action.Investigate, LocationTarget lid) iid _ target _
      | isTarget attrs target -> do
        enemies <- selectList EnemyEngagedWithYou
        a <$ pushAll
          (InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate)
          : [ chooseOne
                iid
                (Label "No evasion" []
                : [ TargetLabel (EnemyTarget enemy) [EnemyEvaded iid enemy]
                  | enemy <- enemies
                  ]
                )
            | notNull enemies
            ]
          )
    _ -> ArchaicGlyphsProphecyForetold3 <$> runMessage msg attrs
