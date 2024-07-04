module Arkham.Location.Cards.TheOnyxCastle (theOnyxCastle, TheOnyxCastle (..)) where

import Arkham.Ability
import Arkham.Helpers.Query (getPlayer)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Placement qualified as Placement

newtype TheOnyxCastle = TheOnyxCastle LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOnyxCastle :: LocationCard TheOnyxCastle
theOnyxCastle = location TheOnyxCastle Cards.theOnyxCastle 3 (Static 0)

instance HasModifiersFor TheOnyxCastle where
  getModifiersFor target (TheOnyxCastle attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Blocked | not attrs.revealed]
  getModifiersFor _ _ = pure []

instance HasAbilities TheOnyxCastle where
  getAbilities (TheOnyxCastle attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (exists (NotYou <> investigatorAt attrs.id) <> exists (TreacheryInHandOf You))
          actionAbility
      ]

instance RunMessage TheOnyxCastle where
  runMessage msg l@(TheOnyxCastle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ not_ (InvestigatorWithId iid) <> investigatorAt attrs
      treacheries <- select $ TreacheryInHandOf $ InvestigatorWithId iid
      player <- getPlayer iid
      chooseOrRunOne
        iid
        [ targetLabel
          treachery
          [ Msg.chooseOrRunOne
              player
              [ targetLabel investigator [PlaceTreachery treachery (Placement.HiddenInHand investigator)]
              | investigator <- investigators
              ]
          ]
        | treachery <- treacheries
        ]

      pure l
    _ -> TheOnyxCastle <$> liftRunMessage msg attrs
