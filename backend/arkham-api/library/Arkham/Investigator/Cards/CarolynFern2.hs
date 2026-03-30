module Arkham.Investigator.Cards.CarolynFern2 (carolynFern2) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CarolynFern2 = CarolynFern2 InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

carolynFern2 :: InvestigatorCard CarolynFern2
carolynFern2 =
  investigator CarolynFern2 Cards.carolynFern2
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 5, combat = 1, agility = 3}

instance HasAbilities CarolynFern2 where
  getAbilities (CarolynFern2 a) =
    [ playerLimit PerRound
        $ withWindowHighlight
        $ selfAbility a 1 (AbleToDiscoverCluesAt YourLocation)
        $ freeReaction
        $ oneOf
          [ AssetHealed #after #horror (#ally <> AssetControlledBy (affectsOthers Anyone)) (SourceOwnedBy You)
          , InvestigatorHealed #after #horror (affectsOthers Anyone) (SourceOwnedBy You)
          ]
    ]

instance HasChaosTokenValue CarolynFern2 where
  getChaosTokenValue iid ElderSign (CarolynFern2 attrs)
    | iid == attrs.id =
        pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CarolynFern2 where
  runMessage msg i@(CarolynFern2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      investigators <- select $ HealableInvestigator (toSource attrs) #horror $ affectsColocated iid
      chooseTargetM iid investigators $ healHorrorOn (ElderSignEffectSource iid) 1
      pure i
    _ -> CarolynFern2 <$> liftRunMessage msg attrs
