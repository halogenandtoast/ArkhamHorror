module Arkham.Asset.Cards.JeromeDavids (
  jeromeDavids,
  JeromeDavids (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardType
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype JeromeDavids = JeromeDavids AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

jeromeDavids :: AssetCard JeromeDavids
jeromeDavids =
  allyWith JeromeDavids Cards.jeromeDavids (1, 4) (isStoryL .~ True)

instance HasModifiersFor JeromeDavids where
  getModifiersFor (InvestigatorTarget iid) (JeromeDavids a) =
    pure [toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( DrawCard
              Timing.When
              You
              (CanCancelRevelationEffect $ BasicCardMatch $ CardWithType TreacheryType)
              EncounterDeck
          )
          (exhaust a <> SkillIconCost 2 (singleton #intellect))
    ]

instance RunMessage JeromeDavids where
  runMessage msg a@(JeromeDavids attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ CancelNext (toSource attrs) RevelationMessage
      pure a
    _ -> JeromeDavids <$> runMessage msg attrs
