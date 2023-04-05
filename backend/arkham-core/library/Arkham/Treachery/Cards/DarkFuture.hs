module Arkham.Treachery.Cards.DarkFuture
  ( darkFuture
  , DarkFuture(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DarkFuture = DarkFuture TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkFuture :: TreacheryCard DarkFuture
darkFuture = treachery DarkFuture Cards.darkFuture

instance HasModifiersFor DarkFuture where
  getModifiersFor (InvestigatorTarget iid) (DarkFuture a) | treacheryOnInvestigator iid a =
    pure $ toModifiers a
         $ map CannotCancelOrIgnoreToken [ElderSign, Skull, Cultist, Tablet, ElderThing, AutoFail]
  getModifiersFor _ _ = pure []

instance HasAbilities DarkFuture where
  getAbilities (DarkFuture a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
      $ ForcedAbility
      $ TurnEnds Timing.After You
    ]

instance RunMessage DarkFuture where
  runMessage msg t@(DarkFuture attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      t <$ push (RequestTokens (toSource attrs) (Just iid) (Reveal 5) SetAside)
    RequestedTokens source (Just iid) (traceShowId -> tokens) | isSource attrs source -> do
      tokenFaces <- getModifiedTokenFaces tokens
      push $ ResetTokens source
      when (ElderSign `elem` tokenFaces) $
        push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
      pure t
    _ -> DarkFuture <$> runMessage msg attrs
