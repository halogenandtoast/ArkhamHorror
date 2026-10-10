module Arkham.Homebrew.AgesUnwound.Locations.DawnOfTheUniverse_201b (
  dawnOfTheUniverse_201b,
) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Strategy

newtype DawnOfTheUniverse_201b = DawnOfTheUniverse_201b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Revelation__ - Put Dawn of the Universe into play." The reverse of
@:ages-unwound:201@; act 4 flips it up and placing it is resolving it.
-}
dawnOfTheUniverse_201b :: LocationCard DawnOfTheUniverse_201b
dawnOfTheUniverse_201b =
  symbolLabel
    $ locationWith DawnOfTheUniverse_201b Cards.dawnOfTheUniverse_201b 4 (PerPlayer 2)
    $ revealedL
    .~ True

{- | "__Forced__ - After you enter Dawn of the Universe: Search the top 9 cards of
your deck for a weakness and draw it. /
__Forced__ - At the end of the round, if there are any clues on this location:
Place 1 doom on the current agenda. Each investigator at this location must move
to a connecting location."
-}
instance HasAbilities DawnOfTheUniverse_201b where
  getAbilities (DawnOfTheUniverse_201b a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Enters #after You (be a)
      , restricted a 2 (thisExists a LocationWithAnyClues) $ forced $ RoundEnds #when
      ]

instance RunMessage DawnOfTheUniverse_201b where
  runMessage msg l@(DawnOfTheUniverse_201b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        iid
        [(FromTopOfDeck 9, ShuffleBackIn)]
        (basic #weakness)
        (DrawFound iid 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeDoomOnAgenda 1
      {- "must move to a connecting location" -- a mandatory move, so the choice
      is only over where. With nowhere legal to go the investigator stays: a move
      that cannot be made is not made. -}
      selectEach (investigatorAt attrs.id) \iid -> do
        destinations <-
          getCanMoveToMatchingLocations iid (attrs.ability 2) (ConnectedFrom ForMovement $ be attrs)
        unless (null destinations)
          $ chooseTargetM iid destinations (moveTo (attrs.ability 2) iid)
      pure l
    _ -> DawnOfTheUniverse_201b <$> liftRunMessage msg attrs
