module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Rapid (rapid) where

import Arkham.Ability
import Arkham.Direction (Direction (Above, Below))
import Arkham.Helpers.Modifiers (ModifierType (Difficulty))
import Arkham.Homebrew.AgainstTheWendigo.Actions (pattern Navigate)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (attachedLocation, inDirectionsOf, isSymbolFace)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (River))
import Arkham.Treachery.Import.Lifted

newtype Rapid = Rapid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rapid :: TreacheryCard Rapid
rapid = treachery Rapid Cards.rapid

instance HasAbilities Rapid where
  getAbilities (Rapid a) =
    [ mkAbility a 1 $ forced $ RoundEnds #when
    , {- "Forced - If an investigator moves into or out of the attached location
      while performing Navigate: The [combat] or [agility] test at stage 4 of
      this action gains +2 difficulty, and +1 damage if you fail."

      Step 3's move is sourced from the Navigate ability, which is what "while
      performing Navigate" reads off, and step 4's test is the next one the
      mover takes. -}
      mkAbility a 2
        $ forced
        $ oneOf
          [ Moves #after You navigating (attachedLocation a) Anywhere
          , Moves #after You navigating Anywhere (attachedLocation a)
          ]
    ]
   where
    navigating = SourceIsAbility (AbilityIsAction Navigate)

instance RunMessage Rapid where
  runMessage msg t@(Rapid attrs) = runQueueT $ case msg of
    -- "Attach Rapid to the nearest location that is both River and Wild."
    Revelation iid (isSource attrs -> True) -> do
      candidates <-
        select
          $ NearestLocationTo iid (LocationWithTrait River <> LocationWithTrait Wild)
      chooseOrRunOneM iid $ targets candidates $ attachTreachery attrs
      pure t
    {- "At the end of the round, reveal a chaos token: on a symbol, attach this
    card to a both River and Wild location directly to the North or South.
    Otherwise discard this card." -}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      requestChaosTokens lead (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      if any isSymbolFace tokens
        then do
          neighbours <-
            select
              $ LocationWithTrait River
              <> LocationWithTrait Wild
              <> inDirectionsOf [Above, Below] attrs
          when (length neighbours < 2) (continue_ iid)
          chooseOrRunOneM iid $ targets neighbours $ attachTreachery attrs
        else do
          continue_ iid
          toDiscard (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nextSkillTestModifier iid (attrs.ability 2) iid (Difficulty 2)
      pure $ Rapid $ setMeta True attrs
    -- "...and +1 damage if you fail", on the test the bump above landed on.
    FailedThisSkillTest iid _ | raised -> do
      assignDamage iid (attrs.ability 2) 1
      pure $ Rapid $ setMeta False attrs
    PassedThisSkillTest _ _ | raised -> pure $ Rapid $ setMeta False attrs
    _ -> Rapid <$> liftRunMessage msg attrs
   where
    raised = toResultDefault False attrs.meta
