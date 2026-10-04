module Arkham.Homebrew.AgainstTheWendigo.Treacheries.UnexpectedObstacles (unexpectedObstacles) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.AgainstTheWendigo.Actions (pattern Navigate, pattern WalkAlongTheRiver)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (isSymbolFace)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype UnexpectedObstacles = UnexpectedObstacles TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedObstacles :: TreacheryCard UnexpectedObstacles
unexpectedObstacles = treachery UnexpectedObstacles Cards.unexpectedObstacles

instance HasModifiersFor UnexpectedObstacles where
  {- | "The first time you perform one of the following actions (Walk Along the
  River, Navigate, move, fight or evade) each round, it costs 1 additional
  action." 'AdditionalActionCostOf' already charges only the first such action
  in a round. -}
  getModifiersFor (UnexpectedObstacles a) =
    inThreatAreaGets
      a
      [ AdditionalActionCostOf (IsAction act) 1
      | act <- [WalkAlongTheRiver, Navigate, #move, #fight, #evade]
      ]

instance HasAbilities UnexpectedObstacles where
  getAbilities (UnexpectedObstacles a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage UnexpectedObstacles where
  runMessage msg t@(UnexpectedObstacles attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    -- "At the end of the round, reveal a chaos token: on a symbol this card
    -- stays in play. Otherwise discard this card."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      requestChaosTokens lead (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) _ tokens -> do
      unless (any isSymbolFace tokens) $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> UnexpectedObstacles <$> liftRunMessage msg attrs
