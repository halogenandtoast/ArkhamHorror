module Arkham.Location.Cards.HemlockChapelNight (hemlockChapelNight) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype HemlockChapelNight = HemlockChapelNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hemlockChapelNight :: LocationCard HemlockChapelNight
hemlockChapelNight = symbolLabel $ location HemlockChapelNight Cards.hemlockChapelNight 4 (PerPlayer 1)

instance HasAbilities HemlockChapelNight where
  getAbilities (HemlockChapelNight a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 (thisExists a LocationWithoutClues)
          $ forced
          $ DiscoverClues #after Anyone (be a) AnyValue
      , playerLimit PerGame
          $ restricted
            a
            2
            ( Here
                <> youExist
                  ( oneOf
                      [ HealableInvestigator (a.ability 2) #horror You
                      , DiscardWith (HasCard #spell) <> can.have.cards.leaveDiscard
                      ]
                  )
            )
            actionAbility
      ]

instance RunMessage HemlockChapelNight where
  runMessage msg l@(HemlockChapelNight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach Anywhere \lid -> placeClues (attrs.ability 1) lid 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      healHorror iid (attrs.ability 2) 1
      search iid (attrs.ability 2) iid [fromDiscard] (basic #spell) (AddFoundToHand iid 1)
      pure l
    _ -> HemlockChapelNight <$> liftRunMessage msg attrs
