module Arkham.Types.Asset.Cards.Kukri
  ( kukri
  , Kukri(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Kukri = Kukri AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kukri :: AssetCard Kukri
kukri = hand Kukri Cards.kukri

instance HasActions Kukri where
  getActions (Kukri a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance HasModifiersFor env Kukri

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasCount ActionRemainingCount env InvestigatorId
  )
  => RunMessage env Kukri where
  runMessage msg a@(Kukri attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid source SkillCombat mempty False
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
