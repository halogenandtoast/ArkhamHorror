module Arkham.Homebrew.AgesUnwound.Enemies.EternitysSentinel_016 (eternitysSentinel_016) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Window qualified as Window

newtype EternitysSentinel_016 = EternitysSentinel_016 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Eternity's Sentinel (Scourge in the Shadows)" prints no health at all --
@*@, with "Cannot be damaged." right under it.

The printed @*@ is 'healthStar' on the card def so the card browser shows it,
but a 'Health' of @*@ evaluates to 0, and 'CheckDefeated' compares
@damage >= health@ -- so any defeat check at all would defeat it on the spot.
Clearing the attr instead (what The Organist, Draped in Mystery does with the
same printout) makes @EnemyHealth@ 'Nothing', which no defeat check can reach.
-}
eternitysSentinel_016 :: EnemyCard EternitysSentinel_016
eternitysSentinel_016 =
  enemyWith EternitysSentinel_016 Cards.eternitysSentinel_016 (healthL .~ Nothing)

-- | "Cannot be damaged."
instance HasModifiersFor EternitysSentinel_016 where
  getModifiersFor (EternitysSentinel_016 a) = modifySelf a [CannotBeDamaged]

{- | Both halves of the first __Forced__ are abilities 1 and 2: the card prints
them as one block, but "After a new Arkham Streets location is put into play,
place Eternity's Sentinel at that location" is a second trigger, and it only
applies while the Sentinel is sitting next to the deck.

Every location put into play during this scenario is a new Arkham Streets
location -- they are the only location cards in it -- so the window needs no
further narrowing.
-}
instance HasAbilities EternitysSentinel_016 where
  getAbilities (EternitysSentinel_016 a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)
      , restricted a 2 (thisExists a $ EnemyWithPlacement Global)
          $ forced
          $ LocationEntersPlay #after Anywhere
      , restricted a 3 (thisExists a $ EnemyWithPlacement Global) $ forced $ RoundEnds #when
      ]

instance RunMessage EternitysSentinel_016 where
  runMessage msg e@(EternitysSentinel_016 attrs) = runQueueT $ case msg of
    {- "Forced - After Eternity's Sentinel attacks: Place it next to the Arkham
    Streets deck (it is still in play, but at no location)."

    'Global' is the engine's in-play-at-no-location placement (Hastur in Dim
    Carcosa, Atlach-Nacha at the centre of its web) and is the one the frontend
    draws in its own row. Engagement needs a shared location, so it is dropped
    explicitly on the way out. -}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      disengageFromAll attrs
      place attrs Global
      pure e
    -- "After a new Arkham Streets location is put into play, place Eternity's
    -- Sentinel at that location."
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let mlid = listToMaybe [lid | (Window.windowType -> Window.LocationEntersPlay lid) <- ws]
      for_ mlid (place attrs . AtLocation)
      pure e
    -- "Forced - At the end of the round, if Eternity's Sentinel is at no
    -- location: Place 1 doom on Eternity's Sentinel."
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      placeDoom (attrs.ability 3) attrs 1
      pure e
    _ -> EternitysSentinel_016 <$> liftRunMessage msg attrs
