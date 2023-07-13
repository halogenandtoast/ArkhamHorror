module Arkham.Treachery.Cards.DarkFuture (
  darkFuture,
  DarkFuture (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.RequestedChaosTokenStrategy
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DarkFuture = DarkFuture TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkFuture :: TreacheryCard DarkFuture
darkFuture = treachery DarkFuture Cards.darkFuture

instance HasModifiersFor DarkFuture where
  getModifiersFor (InvestigatorTarget iid) (DarkFuture a) | treacheryOnInvestigator iid a = do
    pure $
      toModifiers a $
        map CannotCancelOrIgnoreChaosToken [ElderSign, Skull, Cultist, Tablet, ElderThing, AutoFail]
  getModifiersFor _ _ = pure []

instance HasAbilities DarkFuture where
  getAbilities (DarkFuture a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $
        ForcedAbility $
          TurnEnds Timing.After You
    ]

instance RunMessage DarkFuture where
  runMessage msg t@(DarkFuture attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ AttachTreachery (toId attrs) $ InvestigatorTarget iid
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 5) SetAside
      pure t
    RequestedChaosTokens source _ tokens | isSource attrs source -> do
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      push $ ResetChaosTokens source
      when (ElderSign `elem` chaosTokenFaces) $
        push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
      pure t
    _ -> DarkFuture <$> runMessage msg attrs
