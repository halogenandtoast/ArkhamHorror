module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscenceSecrets (reminiscenceSecrets) where

import Arkham.Ability
import Arkham.GameValue (GameValue (Static))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Discover (insteadOfDiscoveringClues)
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ReminiscenceSecrets = ReminiscenceSecrets TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reminiscenceSecrets :: TreacheryCard ReminiscenceSecrets
reminiscenceSecrets = treachery ReminiscenceSecrets Cards.reminiscenceSecrets

instance HasAbilities ReminiscenceSecrets where
  getAbilities (ReminiscenceSecrets a) = case a.placement of
    HiddenInHand iid ->
      [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
      , -- Placing the clue is the cost of the replacement: this card's holder is
        -- the initiator, but the clue comes from the investigator in the window
        -- (ThatInvestigator), so an investigator with no clue to place cannot
        -- trigger it, and skipping costs skips the placement.
        mkAbility a 2
          $ triggered
            (WouldDiscoverClues #when Anyone (locationWithInvestigator iid) AnyValue)
            (InvestigatorPlaceClueOnLocationCost ThatInvestigator (Static 1))
      ]
    _ -> []

instance RunMessage ReminiscenceSecrets where
  runMessage msg t@(ReminiscenceSecrets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      -- The clue was placed as the cost; all that is left is to replace the
      -- discovery itself, which would otherwise still resolve.
      for_ [who | Window _ (Window.WouldDiscoverClues who _ _ _ _) _ <- ws] \who ->
        insteadOfDiscoveringClues who \_ -> pure ()
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ReminiscenceSecrets <$> liftRunMessage msg attrs
