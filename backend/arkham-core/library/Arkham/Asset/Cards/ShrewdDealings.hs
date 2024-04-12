module Arkham.Asset.Cards.ShrewdDealings (shrewdDealings, ShrewdDealings (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Card
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Modifier

newtype ShrewdDealings = ShrewdDealings AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrewdDealings :: AssetCard ShrewdDealings
shrewdDealings = asset ShrewdDealings Cards.shrewdDealings

instance HasModifiersFor ShrewdDealings where
  getModifiersFor (InvestigatorTarget iid) (ShrewdDealings attrs) | iid `controls` attrs = do
    pure $ toModifiers attrs [ReduceCostOf (#asset <> #item) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities ShrewdDealings where
  getAbilities (ShrewdDealings x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> exists (affectsOthers $ InvestigatorAt YourLocation <> NotYou))
        $ freeReaction (PlayCard #when You $ basic $ #asset <> #item <> NonSignature)
    ]

instance RunMessage ShrewdDealings where
  runMessage msg a@(ShrewdDealings attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      investigators <- select $ affectsOthers $ colocatedWith iid
      chooseOne
        iid
        [ targetLabel
          investigator
          [ Msg.cardResolutionModifier
              card
              (attrs.ability 1)
              (CardIdTarget $ toCardId card)
              (PlayUnderControlOf investigator)
          ]
        | investigator <- investigators
        ]
      pure a
    _ -> ShrewdDealings <$> lift (runMessage msg attrs)
