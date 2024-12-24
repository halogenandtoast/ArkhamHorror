module Arkham.Location.Cards.RiverviewTheatre (riverviewTheatre) where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype RiverviewTheatre = RiverviewTheatre LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverviewTheatre :: LocationCard RiverviewTheatre
riverviewTheatre = location RiverviewTheatre Cards.riverviewTheatre 1 (PerPlayer 4)

mirageCards :: [CardDef]
mirageCards = [Cards.drKenslersOffice, Cards.infirmaryFatalMirage]

instance HasModifiersFor RiverviewTheatre where
  getModifiersFor (RiverviewTheatre a) = clearedOfMirages a mirageCards

instance HasAbilities RiverviewTheatre where
  getAbilities (RiverviewTheatre a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , restricted a 1 (youExist $ at_ (be a))
          $ forced
          $ DrawCard #when You (basic $ CardFromEncounterSet Tekelili) AnyDeck
      ]

instance RunMessage RiverviewTheatre where
  runMessage msg l@(RiverviewTheatre attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cardResolutionModifier card (attrs.ability 1) card ResolveEffectsAgain
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 1
      pure l
    _ -> RiverviewTheatre <$> mirageRunner Stories.riverviewTheatre mirageCards 2 msg attrs
