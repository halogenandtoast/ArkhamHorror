module Arkham.Location.Cards.TracesToNowhere.EtherealPerch (etherealPerch) where

import Arkham.Ability
import Arkham.Helpers.Query
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Otherworld))

newtype EtherealPerch = EtherealPerch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealPerch :: LocationCard EtherealPerch
etherealPerch = location EtherealPerch Cards.etherealPerch 3 (PerPlayer 1)

instance HasAbilities EtherealPerch where
  getAbilities (EtherealPerch a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ RoundEnds #when
      ]

instance RunMessage EtherealPerch where
  runMessage msg l@(EtherealPerch attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      setAside <- getSetAsideCardsMatching (#location <> CardWithTrait Otherworld)
      for_ setAside placeLocation_
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeCluesUpToClueValue (attrs.ability 2) attrs
      pure l
    _ -> EtherealPerch <$> liftRunMessage msg attrs
