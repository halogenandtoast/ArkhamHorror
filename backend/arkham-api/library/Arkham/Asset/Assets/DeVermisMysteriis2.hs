module Arkham.Asset.Assets.DeVermisMysteriis2 (
  deVermisMysteriis2,
  DeVermisMysteriis2 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
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
    <> basic (#event <> oneOf [#spell, #insight])

instance RunMessage DeVermisMysteriis2 where
  runMessage msg a@(DeVermisMysteriis2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      let windows'' = nub $ windows' <> [mkWhen (Window.DuringTurn iid), mkWhen Window.FastPlayerWindow]
      cards <- select $ cardMatcher (InvestigatorWithId iid)

      chooseOneM iid do
        targets cards \card -> do
          costModifier attrs card $ ReduceCostOf (CardWithId card.id) 1
          eventModifier attrs card RemoveFromGameInsteadOfDiscard
          playCardPayingCostWithWindows iid card windows''
          removeCardFromGame card
      pure a
    _ -> DeVermisMysteriis2 <$> liftRunMessage msg attrs
