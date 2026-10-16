module Arkham.Homebrew.TheInvisibleMan.Treacheries.UnseenMovements (unseenMovements) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype UnseenMovements = UnseenMovements TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unseenMovements :: TreacheryCard UnseenMovements
unseenMovements = treachery UnseenMovements Cards.unseenMovements

instance HasModifiersFor UnseenMovements where
  getModifiersFor (UnseenMovements a) = do
    modifySelect a Anyone [AdditionalCostToEnterMatching griffinsLocation discardACard]
    modifySelect a griffinsLocation [AdditionalCostToLeave discardACard]
   where
    griffinsLocation = LocationWithEnemy griffin
    discardACard = HandDiscardCost 1 #any

instance HasAbilities UnseenMovements where
  getAbilities (UnseenMovements a) =
    [ scenarioI18n
        $ withI18nTooltip "unseenMovements.discard"
        $ mkAbility a 1
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage UnseenMovements where
  runMessage msg t@(UnseenMovements attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> UnseenMovements <$> liftRunMessage msg attrs
