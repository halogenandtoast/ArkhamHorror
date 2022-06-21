module Arkham.Event.Cards.AstralTravel
  ( astralTravel
  , AstralTravel(..)
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Runner
import Arkham.Matcher hiding (MoveAction)
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Target
import Arkham.Token
import Arkham.Trait qualified as Trait

newtype AstralTravel = AstralTravel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astralTravel :: EventCard AstralTravel
astralTravel = event AstralTravel Cards.astralTravel

instance RunMessage AstralTravel where
  runMessage msg e@(AstralTravel attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      locations <- selectList $ RevealedLocation <> Unblocked <> NotYourLocation
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel (LocationTarget lid) [MoveAction iid lid Free False]
          | lid <- locations
          ]
        , RequestTokens (toSource attrs) Nothing (Reveal 1) SetAside
        , Discard (toTarget attrs)
        ]
    RequestedTokens source _ tokens | isSource attrs source -> e <$ when
      (any
        ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . tokenFace)
        tokens
      )
      do
        targets <- selectList
          $ AssetOneOf (AssetWithTrait <$> [Trait.Item, Trait.Ally])
        case targets of
          [] -> push
            (InvestigatorAssignDamage (eventOwner attrs) source DamageAny 1 0)
          xs ->
            push (chooseOne (eventOwner attrs) (Discard . AssetTarget <$> xs))
    _ -> AstralTravel <$> runMessage msg attrs
