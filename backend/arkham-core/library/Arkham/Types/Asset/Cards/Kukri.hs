module Arkham.Types.Asset.Cards.Kukri
  ( kukri
  , Kukri(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Kukri = Kukri AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kukri :: AssetCard Kukri
kukri = hand Kukri Cards.kukri

instance HasAbilities Kukri where
  getAbilities (Kukri a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance AssetRunner env => RunMessage env Kukri where
  runMessage msg a@(Kukri attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        actionRemainingCount <- unActionRemainingCount <$> getCount iid
        a <$ when
          (actionRemainingCount > 0)
          (push $ chooseOne
            iid
            [ Label
              "Spend 1 action to deal +1 damage"
              [ LoseActions iid source 1
              , skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
              ]
            , Label "Skip additional Kukri damage" []
            ]
          )
    _ -> Kukri <$> runMessage msg attrs
