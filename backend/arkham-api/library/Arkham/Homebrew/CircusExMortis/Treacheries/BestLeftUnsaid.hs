module Arkham.Homebrew.CircusExMortis.Treacheries.BestLeftUnsaid (bestLeftUnsaid) where

import Arkham.Ability
import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement (Placement (..), place)
import Arkham.Trait (Trait (Restricted))
import Arkham.Treachery.Import.Lifted hiding (PerformAction)

newtype BestLeftUnsaid = BestLeftUnsaid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bestLeftUnsaid :: TreacheryCard BestLeftUnsaid
bestLeftUnsaid = treachery BestLeftUnsaid Cards.bestLeftUnsaid

-- The parley trigger is not restricted to [[Restricted]] locations; only the
-- end-of-turn one is.
instance HasAbilities BestLeftUnsaid where
  getAbilities (BestLeftUnsaid a) =
    [ mkAbility a 1
        $ forced
        $ oneOf
          [ PerformAction #after You #parley
          , TurnEnds #after (You <> at_ (LocationWithTrait Restricted))
          ]
    , mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage BestLeftUnsaid where
  runMessage msg t@(BestLeftUnsaid attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canPlaceCluesOnYourLocation iid >>= \case
        True -> placeCluesOnLocation iid (attrs.ability 1) 1
        False -> assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> BestLeftUnsaid <$> liftRunMessage msg attrs
