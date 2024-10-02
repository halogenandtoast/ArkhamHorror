module Arkham.Event.Events.ShrineOfTheMoirai3 (shrineOfTheMoirai3, ShrineOfTheMoirai3 (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
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

instance HasAbilities ShrineOfTheMoirai3 where
  getAbilities (ShrineOfTheMoirai3 x) = case x.placement of
    AtLocation lid ->
      [ restrictedAbility (proxied lid x) 1 Here
          $ FastAbility (exhaust x <> EventUseCost (be x) Offering 1 <> DrawEncounterCardsCost 1)
      ]
    _ -> []

instance RunMessage ShrineOfTheMoirai3 where
  runMessage msg e@(ShrineOfTheMoirai3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      place attrs =<< getJustLocation iid
      pure . ShrineOfTheMoirai3 $ attrs & tokensL . at Offering .~ Just 3
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      cs <- select $ inDiscardOf iid <> basic (CardWithMaxLevel 5)
      when (notNull cs) do
        chooseOne
          iid
          [ targetLabel c
            $ Msg.addToHand iid c
            : [DoStep (5 - fromJustNote "missing level" c.level) msg]
          | c <- cs
          ]
      pure e
    DoStep n (UseThisAbility iid (isProxySource attrs -> True) 1) -> do
      cs <- select $ inDiscardOf iid <> basic (CardWithMaxLevel n)
      chooseOrRunOne iid
        $ Label "Done choosing cards" []
        : [targetLabel c [Msg.addToHand iid c] | c <- cs]
      pure e
    _ -> ShrineOfTheMoirai3 <$> liftRunMessage msg attrs
