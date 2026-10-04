module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Rapid (rapid) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (isSymbolFace)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (River))
import Arkham.Treachery.Import.Lifted

newtype Rapid = Rapid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Rapid raises the fourth step of a Navigate action into or out of the
location it is attached to. That step is a test the location's Navigate ability
starts, so the difficulty bump is applied there rather than here; what this card
owns is where it sits and when it moves on.
-}
rapid :: TreacheryCard Rapid
rapid = treachery Rapid Cards.rapid

instance HasAbilities Rapid where
  getAbilities (Rapid a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage Rapid where
  runMessage msg t@(Rapid attrs) = runQueueT $ case msg of
    -- "Attach Rapid to the nearest location that is both River and Wild."
    Revelation iid (isSource attrs -> True) -> do
      candidates <-
        select
          $ NearestLocationTo iid (LocationWithTrait River <> LocationWithTrait Wild)
      chooseOrRunOneM iid $ targets candidates $ attachTreachery attrs
      pure t
    {- | "At the end of the round, reveal a chaos token: on a symbol, attach this
    card to a both River and Wild location directly to the North or South.
    Otherwise discard this card." -}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      requestChaosTokens lead (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      if any isSymbolFace tokens
        then do
          neighbours <- case attrs.attached.location of
            Nothing -> pure []
            Just lid ->
              select
                $ connectedFrom (LocationWithId lid)
                <> LocationWithTrait River
                <> LocationWithTrait Wild
          chooseOrRunOneM iid $ targets neighbours $ attachTreachery attrs
        else toDiscard (attrs.ability 1) attrs
      pure t
    _ -> Rapid <$> liftRunMessage msg attrs
