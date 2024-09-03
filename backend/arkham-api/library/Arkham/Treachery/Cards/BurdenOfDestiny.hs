module Arkham.Treachery.Cards.BurdenOfDestiny (burdenOfDestiny, BurdenOfDestiny (..)) where

import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Unbroken))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurdenOfDestiny = BurdenOfDestiny TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdenOfDestiny :: TreacheryCard BurdenOfDestiny
burdenOfDestiny = treachery BurdenOfDestiny Cards.burdenOfDestiny

instance RunMessage BurdenOfDestiny where
  runMessage msg t@(BurdenOfDestiny attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      disciplines <- select $ AssetWithTitle "Discipline" <> AssetWithTrait Unbroken
      player <- getPlayer iid
      chooseOrRunOne iid
        $ [ Label
            "Flip a Discipline you control to its Broken side. It cannot be flipped back this round."
            [ Msg.chooseOrRunOne
                player
                [ targetLabel
                  discipline
                  [ Flip iid (toSource attrs) (toTarget discipline)
                  , Msg.roundModifier attrs discipline CannotBeFlipped
                  ]
                | discipline <- disciplines
                ]
            ]
          | notNull disciplines
          ]
        <> [Label "Take 1 damage and 1 horror." [Msg.assignDamageAndHorror iid attrs 1 1]]

      pure t
    _ -> BurdenOfDestiny <$> liftRunMessage msg attrs
