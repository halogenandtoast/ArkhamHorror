module Arkham.Asset.Cards.DeVermisMysteriis2 (
  deVermisMysteriis2,
  DeVermisMysteriis2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Trait (Trait (Insight, Spell))
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype DeVermisMysteriis2 = DeVermisMysteriis2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deVermisMysteriis2 :: AssetCard DeVermisMysteriis2
deVermisMysteriis2 = asset DeVermisMysteriis2 Cards.deVermisMysteriis2

instance HasAbilities DeVermisMysteriis2 where
  getAbilities (DeVermisMysteriis2 a) =
    [ controlledAbility a 1 (exists $ cardMatcher You)
        $ actionAbilityWithCost (exhaust a <> DoomCost (toSource a) (toTarget a) 1)
    ]

cardMatcher :: InvestigatorMatcher -> ExtendedCardMatcher
cardMatcher who =
  PlayableCardWithCostReduction NoAction 1
    $ InDiscardOf who
    <> BasicCardMatch (#event <> CardWithOneOf [CardWithTrait Spell, CardWithTrait Insight])

instance RunMessage DeVermisMysteriis2 where
  runMessage msg a@(DeVermisMysteriis2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      let windows'' = nub $ windows' <> [mkWhen (Window.DuringTurn iid), mkWhen Window.FastPlayerWindow]
      cards <- select $ cardMatcher (InvestigatorWithId iid)

      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel
            (toCardId card)
            [ costModifier attrs (toCardId card) (ReduceCostOf (CardWithId $ toCardId card) 1)
            , eventModifier attrs (toCardId card) RemoveFromGameInsteadOfDiscard
            , AddToHand iid [card]
            , PayCardCost iid card windows''
            , RemoveFromGame (CardIdTarget $ toCardId card)
            ]
          | card <- cards
          ]
      pure a
    _ -> DeVermisMysteriis2 <$> runMessage msg attrs
