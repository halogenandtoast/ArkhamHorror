module Arkham.Homebrew.DarkMatter.Treacheries.RememberME (rememberME) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Treachery.Import.Lifted

newtype RememberME = RememberME TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rememberME :: TreacheryCard RememberME
rememberME = treachery RememberME Cards.rememberME

{- | "Hidden. Peril. Revelation - Secretly add this card to your hand.
Forced - At the end of the enemy phase, if The Boogeyman is at your location or
an adjacent location, search your discard pile for a weakness and draw it. Then,
discard this card."
-}
instance HasAbilities RememberME where
  getAbilities (RememberME a) =
    [ restricted
        a
        1
        ( youExist
            $ at_
            $ oneOf
              [ LocationWithEnemy (enemyIs Enemies.theBOOGEYMAN)
              , connectedFrom (LocationWithEnemy $ enemyIs Enemies.theBOOGEYMAN)
              ]
        )
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage RememberME where
  runMessage msg t@(RememberME attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDiscard] (basic WeaknessCard) (DrawFound iid 1)
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> RememberME <$> liftRunMessage msg attrs
