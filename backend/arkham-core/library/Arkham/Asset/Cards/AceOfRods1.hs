module Arkham.Asset.Cards.AceOfRods1
  ( aceOfRods1
  , aceOfRods1Effect
  , AceOfRods1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher hiding ( DuringTurn )
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window ( defaultWindows )

newtype AceOfRods1 = AceOfRods1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceOfRods1 :: AssetCard AceOfRods1
aceOfRods1 = asset AceOfRods1 Cards.aceOfRods1

instance HasAbilities AceOfRods1 where
  getAbilities (AceOfRods1 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringTurn You) $ FastAbility Free
    , restrictedAbility a 2 InYourHand
      $ ReactionAbility (GameBegins Timing.When) Free
    ]

instance RunMessage AceOfRods1 where
  runMessage msg a@(AceOfRods1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ RemoveFromGame (toTarget attrs)
        , createCardEffect
          Cards.aceOfRods1
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        ]
      pure a
    InHand _ (UseCardAbility iid (isSource attrs -> True) 2 _ _) -> do
      push (PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid))
      pure a
    _ -> AceOfRods1 <$> runMessage msg attrs

newtype Meta = Meta { active :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AceOfRods1Effect = AceOfRods1Effect (EffectAttrs `With` Meta)
  deriving anyclass (IsEffect, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceOfRods1Effect :: EffectArgs -> AceOfRods1Effect
aceOfRods1Effect =
  cardEffect (AceOfRods1Effect . (`with` Meta False)) Cards.aceOfRods1

instance HasModifiersFor AceOfRods1Effect where
  getModifiersFor target (AceOfRods1Effect (a `With` meta))
    | target == effectTarget a && active meta = pure
    $ toModifiers a [ SkillModifier sType 2 | sType <- allSkills ]
  getModifiersFor _ _ = pure []

instance RunMessage AceOfRods1Effect where
  runMessage msg e@(AceOfRods1Effect (attrs@EffectAttrs {..} `With` meta)) =
    case msg of
      CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == toId attrs -> do
        push
          $ GainAdditionalAction iid (toSource attrs)
          $ EffectAction
              "Use Ace of Rods (1) extra action with +2 to each skill"
          $ toId attrs
        pure e
      UseEffectAction iid eid _ | eid == effectId -> do
        push $ GainActions iid (toSource attrs) 1
        pure $ AceOfRods1Effect (attrs `with` Meta True)
      FinishAction-> do
        when (active meta) $ push $ DisableEffect effectId
        pure e
      EndTurn iid | InvestigatorTarget iid == effectTarget -> do
        push $ DisableEffect effectId
        pure e
      _ -> AceOfRods1Effect . (`with` meta) <$> runMessage msg attrs
