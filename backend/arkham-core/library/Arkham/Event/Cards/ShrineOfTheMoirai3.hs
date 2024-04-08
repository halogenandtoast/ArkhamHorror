module Arkham.Event.Cards.ShrineOfTheMoirai3 (shrineOfTheMoirai3, ShrineOfTheMoirai3 (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Placement

newtype ShrineOfTheMoirai3 = ShrineOfTheMoirai3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrineOfTheMoirai3 :: EventCard ShrineOfTheMoirai3
shrineOfTheMoirai3 = event ShrineOfTheMoirai3 Cards.shrineOfTheMoirai3

-- Draw the top card of the encounter deck, exhaust Shrine of the Moirai, and spend 1 offering: Return up to 2 cards with a total combined level of 5 or less from your discard pile to your hand. Any investigator at this location may trigger this ability
instance HasAbilities ShrineOfTheMoirai3 where
  getAbilities (ShrineOfTheMoirai3 x) = case x.placement of
    AtLocation lid ->
      [ restrictedAbility
          (proxy (LocationSource lid) x)
          1
          Here
          $ FastAbility (exhaust x <> EventUseCost (EventWithId x.id) Offering 1 <> DrawEncounterCardsCost 1)
      ]
    _ -> []

instance RunMessage ShrineOfTheMoirai3 where
  runMessage msg e@(ShrineOfTheMoirai3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      location <- getJustLocation iid
      push $ PlaceEvent iid eid $ AtLocation location
      pure . ShrineOfTheMoirai3 $ attrs & usesL . at Offering .~ Just 3
    UseThisAbility _iid (isProxySource attrs -> True) 1 -> do
      push $ DoStep 5 msg
      pure e
    DoStep n msg'@(UseThisAbility iid (isProxySource attrs -> True) 1) -> do
      targets <- select $ InDiscardOf (InvestigatorWithId iid) <> basic (CardWithMaxLevel n)
      when (notNull targets) do
        chooseOne
          iid
          [ TargetLabel
            (CardIdTarget $ toCardId target)
            [Msg.addToHand iid target, DoStep (n - fromJustNote "missing level" target.level) msg']
          | target <- targets
          ]
      pure e
    _ -> ShrineOfTheMoirai3 <$> lift (runMessage msg attrs)
