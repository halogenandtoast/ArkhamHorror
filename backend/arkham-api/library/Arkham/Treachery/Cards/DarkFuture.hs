module Arkham.Treachery.Cards.DarkFuture (darkFuture, DarkFuture (..)) where

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DarkFuture = DarkFuture TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkFuture :: TreacheryCard DarkFuture
darkFuture = treachery DarkFuture Cards.darkFuture

instance HasModifiersFor DarkFuture where
  getModifiersFor (DarkFuture a) = do
    inThreatAreaGets a
      $ map CannotCancelOrIgnoreChaosToken [ElderSign, Skull, Cultist, Tablet, ElderThing, AutoFail]

instance HasAbilities DarkFuture where
  getAbilities (DarkFuture a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ TurnEnds #after You
    ]

instance RunMessage DarkFuture where
  runMessage msg t@(DarkFuture attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 5) SetAside
      pure t
    RequestedChaosTokens source (Just iid) tokens | isSource attrs source -> do
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      push $ ResetChaosTokens source
      pushWhen (ElderSign `elem` chaosTokenFaces)
        $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      player <- getPlayer iid
      push $ chooseOne player [Label "Continue" []]
      pure t
    _ -> DarkFuture <$> runMessage msg attrs
