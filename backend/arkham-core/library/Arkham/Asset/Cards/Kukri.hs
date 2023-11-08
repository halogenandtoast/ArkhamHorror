module Arkham.Asset.Cards.Kukri (
  kukri,
  Kukri (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType

newtype Kukri = Kukri AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kukri :: AssetCard Kukri
kukri = asset Kukri Cards.kukri

instance HasAbilities Kukri where
  getAbilities (Kukri a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight]) (ActionCost 1)
    ]

instance RunMessage Kukri where
  runMessage msg a@(Kukri attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifier
            attrs
            (InvestigatorTarget iid)
            (SkillModifier SkillCombat 1)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      player <- getPlayer iid
      pushWhen (actionRemainingCount > 0)
        $ chooseOne
          player
          [ Label
              "Spend 1 action to deal +1 damage"
              [ LoseActions iid source 1
              , skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
              ]
          , Label "Skip additional Kukri damage" []
          ]
      pure a
    _ -> Kukri <$> runMessage msg attrs
