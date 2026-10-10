module Arkham.Homebrew.AgesUnwound.Treacheries.SheerExhaustion (sheerExhaustion) where

import Arkham.Ability
import Arkham.GameEnv (getHistoryField, getPhase)
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.History
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Phase
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype SheerExhaustion = SheerExhaustion TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sheerExhaustion :: TreacheryCard SheerExhaustion
sheerExhaustion = treachery SheerExhaustion Cards.sheerExhaustion

-- | "You cannot draw cards or gain resources during the upkeep phase."
instance HasModifiersFor SheerExhaustion where
  getModifiersFor (SheerExhaustion a) = case a.placement of
    InThreatArea iid -> do
      phase <- getPhase
      modifiedWhen_ a (phase == UpkeepPhase) iid [CannotDrawCards, CannotGainResources]
    _ -> pure ()

{- | "Forced - At the end of your turn, if you have not moved this round: Discard
Sheer Exhaustion."

The window is narrowed with the turn-scoped matcher the engine has, and the
printed /round/ condition is then checked for real in the handler --
'InvestigatorThatMovedDuringTurn' would let a move made outside your own turn
(Just Business during the mythos phase) go unnoticed.
TODO(ages-unwound): a round-scoped "moved" matcher belongs in the shared
'Arkham.Matcher.History'; propose it rather than adding one here.
-}
instance HasAbilities SheerExhaustion where
  getAbilities (SheerExhaustion a) =
    [ restricted a 1 InYourThreatArea
        $ forced
        $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)
    ]

instance RunMessage SheerExhaustion where
  runMessage msg t@(SheerExhaustion attrs) = runQueueT $ case msg of
    -- "Revelation - Put Sheer Exhaustion into play in your threat area."
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moved <- getHistoryField RoundHistory iid HistoryMoved
      when (moved == 0) $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> SheerExhaustion <$> liftRunMessage msg attrs
