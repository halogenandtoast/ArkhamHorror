module Arkham.Homebrew.AgesUnwound.Enemies.EntropicShade (entropicShade) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Matcher
import Arkham.Placement
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype EntropicShade = EntropicShade EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | __Alert__ and __Hunter__ are printed keywords and come off the card def.
entropicShade :: EnemyCard EntropicShade
entropicShade = enemy EntropicShade Cards.entropicShade

{- | "__Forced__ - When an ability printed on an asset deals damage to Entropic
Shade, if that asset has no copy of Accelerated Decay attached: Search the
encounter deck and discard pile for a copy of Accelerated Decay and attach it to
that asset. Shuffle the encounter deck."

The asset is only knowable from the window, so "has no copy attached" is re-read
in the handler rather than written as an ability criterion. The search routes its
answer at the asset itself ('Arkham.Target.AssetTarget'), which is how the
handler still knows where to attach the copy it found.
-}
instance HasAbilities EntropicShade where
  getAbilities (EntropicShade a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceIsAsset AnyAsset)

instance RunMessage EntropicShade where
  runMessage msg e@(EntropicShade attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      let sources = [source | (windowType -> Window.DealtDamage source _ _ _) <- ws]
      for_ (mapMaybe (.asset) sources) \aid -> do
        marked <-
          aid <=~> AssetWithAttachedTreachery (treacheryIs Treacheries.acceleratedDecay)
        unless marked do
          lead <- getLead
          findEncounterCard lead (AssetTarget aid) (cardIs Treacheries.acceleratedDecay)
      pure e
    {- The search's own shuffle is what "Shuffle the encounter deck" asks for;
    placing the treachery attached rather than drawing it is deliberate -- the
    copy is attached by this card, not revealed, so its own Revelation (which
    would let the investigator pick the asset) must not run. -}
    FoundEncounterCard _ (AssetTarget aid) (toCard -> card)
      | card `cardMatch` cardIs Treacheries.acceleratedDecay -> do
          createTreacheryAt_ card (AttachedToAsset aid Nothing)
          pure e
    _ -> EntropicShade <$> liftRunMessage msg attrs
