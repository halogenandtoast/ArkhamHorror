module Arkham.Campaigns.TheDrownedCity.Effects.WalkInFaithResolve where

import Arkham.Ability
import Arkham.Classes.Entity (Entity)
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.RunMessage.Internal (RunMessage (..), liftRunMessage)
import Arkham.Draw.Types (CardDraw (..), CardDrawRules (..))
import Arkham.Effect.Import
import Arkham.Effect.Types qualified as Effect
import Arkham.Investigator.Types (Field (InvestigatorDrawing))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message (Message (..), pattern UseThisAbility)
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection (field)
import Arkham.Source (Sourceable (..))

{- | Walk in Faith, "I am firm in my resolve": "The first time each investigator
draws an encounter card in the next scenario, that encounter card gains surge."

One of these is created per investigator during The Apiary's intro — "the next
scenario" in a Task story read before setup is the scenario being set up.

Hooked on the /would draw/ window because that is the last point at which the draw
can still be edited. Rather than replacing the pending draw outright (which would
lose its amount, deck and continuation), read it back off the investigator and add
the surge rule to it, which is the same seam Temple of R'lyeh uses when it initiates
a draw itself.
-}
newtype WalkInFaithResolveEffect = WalkInFaithResolveEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walkInFaithResolveEffect :: EffectArgs -> WalkInFaithResolveEffect
walkInFaithResolveEffect (effectId, builder) =
  WalkInFaithResolveEffect $ Effect.baseAttrs "walkInFaithResolve" effectId builder

instance HasAbilities WalkInFaithResolveEffect where
  getAbilities (WalkInFaithResolveEffect a) = case a.target.investigator of
    Nothing -> []
    Just iid -> [mkAbility a 1 $ SilentForcedAbility $ WouldDrawEncounterCard #when (be iid) #any]

instance RunMessage WalkInFaithResolveEffect where
  runMessage msg e@(WalkInFaithResolveEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDrawing <- field InvestigatorDrawing iid
      for_ mDrawing \drawing -> do
        let surge = WithDrawnCardModifiers (toSource attrs) [AddKeyword Keyword.Surge]
        push
          $ ReplaceCurrentCardDraw iid
          $ drawing {cardDrawRules = insertSet surge drawing.cardDrawRules}
      disableReturn e
    _ -> WalkInFaithResolveEffect <$> liftRunMessage msg attrs
