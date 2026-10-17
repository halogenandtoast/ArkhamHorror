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

instance HasModifiersFor SheerExhaustion where
  getModifiersFor (SheerExhaustion a) = case a.placement of
    InThreatArea iid -> do
      phase <- getPhase
      modifiedWhen_ a (phase == UpkeepPhase) iid [CannotDrawCards, CannotGainResources]
    _ -> pure ()

instance HasAbilities SheerExhaustion where
  getAbilities (SheerExhaustion a) =
    [ restricted a 1 InYourThreatArea
        $ forced
        $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)
    ]

instance RunMessage SheerExhaustion where
  runMessage msg t@(SheerExhaustion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moved <- getHistoryField RoundHistory iid HistoryMoved
      when (moved == 0) $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> SheerExhaustion <$> liftRunMessage msg attrs
