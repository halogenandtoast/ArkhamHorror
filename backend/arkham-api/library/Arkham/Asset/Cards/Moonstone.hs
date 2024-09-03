module Arkham.Asset.Cards.Moonstone (
  moonstone,
  Moonstone (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (Discarded)
import Arkham.Card
import Arkham.Matcher

newtype Moonstone = Moonstone AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonstone :: AssetCard Moonstone
moonstone = asset Moonstone Cards.moonstone

instance HasModifiersFor Moonstone where
  getModifiersFor (InvestigatorTarget iid) (Moonstone a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #willpower 1, SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities Moonstone where
  getAbilities (Moonstone x) =
    [ restrictedAbility x 1 InYourDiscard
        $ freeReaction
          ( Discarded #after (Just You) AnySource
              $ PlayableCardWithCriteria NeedsAction (CriteriaOverride NoRestriction)
              $ basic
              $ CardWithId
              $ toCardId x
          )
    ]

instance RunMessage Moonstone where
  runMessage msg a@(Moonstone attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      pushAll
        [ PayCardCost iid (toCard attrs) windows'
        , PutCardIntoPlay iid (toCard attrs) Nothing NoPayment windows'
        ]
      pure a
    _ -> Moonstone <$> runMessage msg attrs
