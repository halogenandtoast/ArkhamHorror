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
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype Kukri = Kukri AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kukri :: AssetCard Kukri
kukri = hand Kukri Cards.kukri

instance ActionRunner env => HasActions env Kukri where
  getActions iid NonFast (Kukri a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) (ActionCost 1))
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env Kukri where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasCount ActionRemainingCount env InvestigatorId
  )
  => RunMessage env Kukri where
  runMessage msg a@(Kukri attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        actionRemainingCount <- unActionRemainingCount <$> getCount iid
        if actionRemainingCount > 0
          then a <$ unshiftMessage
            (chooseOne
              iid
              [ Label
                "Spend 1 action to deal +1 damage"
                [ LoseActions iid source 1
                , CreateWindowModifierEffect
                  EffectSkillTestWindow
                  (EffectModifiers $ toModifiers attrs [DamageDealt 1])
                  source
                  (InvestigatorTarget iid)
                ]
              , Label "Skip additional Kukri damage" []
              ]
            )
          else pure a
    _ -> Kukri <$> runMessage msg attrs
