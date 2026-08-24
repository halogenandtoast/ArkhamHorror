module Arkham.Homebrew.DarkMatter.Treacheries.CloseEncounters (closeEncounters) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted hiding (RevealChaosToken)

newtype CloseEncounters = CloseEncounters TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeEncounters :: TreacheryCard CloseEncounters
closeEncounters = treachery CloseEncounters Cards.closeEncounters

{- | "Forced - When you reveal a chaos token while attacking or evading an enemy:
Discard Close Encounters and reveal an additional chaos token."
-}
instance HasAbilities CloseEncounters where
  getAbilities (CloseEncounters a) =
    [ restricted
        a
        1
        ( InThreatAreaOf You
            <> DuringSkillTest
              (YourSkillTest $ oneOf [WhileAttackingAnEnemy AnyEnemy, WhileEvadingAnEnemy AnyEnemy])
        )
        $ forced
        $ RevealChaosToken #when You AnyChaosToken
    ]

instance RunMessage CloseEncounters where
  runMessage msg t@(CloseEncounters attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    {- The test is already past its reveal step here, so the reveal-strategy
    modifier would never be read; draw the extra token directly instead. -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid attrs attrs
      drawAnotherChaosToken iid
      pure t
    _ -> CloseEncounters <$> liftRunMessage msg attrs
