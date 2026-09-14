module Arkham.Homebrew.DarkMatter.Treacheries.BrokenReality (brokenReality) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (caveOrCarcosaLocation)
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype BrokenReality = BrokenReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenReality :: TreacheryCard BrokenReality
brokenReality = treachery BrokenReality Cards.brokenReality

{- | "Forced - After you flip the attached location: Flip it back to its original
side and discard this card." / "[action] Spend 1 clue: Discard this card."
-}
instance HasAbilities BrokenReality where
  getAbilities (BrokenReality a) =
    [ mkAbility a 1
        $ forced
        $ FlipLocation #after Anyone (maybe Nowhere LocationWithId a.attached.location)
    , restricted a 2 (youExist $ InvestigatorWithClues $ atLeast 1)
        $ actionAbilityWithCost (ClueCost $ Static 1)
    ]

instance RunMessage BrokenReality where
  runMessage msg t@(BrokenReality attrs) = runQueueT $ case msg of
    -- "Revelation - Attach this card to a [[Cave]] or [[Carcosa]] location with
    -- the fewest clues without a copy of Broken Reality attached."
    Revelation iid (isSource attrs -> True) -> do
      -- no "fewest clues" location matcher exists, so narrow to the minimum here
      candidates <-
        select
          $ caveOrCarcosaLocation
          <> not_ (LocationWithTreachery $ treacheryIs Cards.brokenReality)
      withClues <- for candidates \lid -> (lid,) <$> field LocationClues lid
      let fewest = [lid | (lid, n) <- withClues, n == minimumEx (map snd withClues)]
      if null withClues
        then toDiscard attrs attrs
        else chooseTargetM iid fewest $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- discard first: the flip back opens another 'FlipLocation' window, which
      -- this same forced ability would answer again if the card were still attached
      toDiscardBy iid attrs attrs
      for_ attrs.attached.location $ push . Flip iid (toSource attrs) . toTarget
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> BrokenReality <$> liftRunMessage msg attrs
