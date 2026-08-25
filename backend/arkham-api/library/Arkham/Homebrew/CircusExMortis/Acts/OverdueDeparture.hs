module Arkham.Homebrew.CircusExMortis.Acts.OverdueDeparture (overdueDeparture) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Matcher

campOutskirts :: LocationMatcher
campOutskirts = LocationWithTitle "Camp Outskirts"

newtype OverdueDeparture = OverdueDeparture ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overdueDeparture :: ActCard OverdueDeparture
overdueDeparture = act (2, A) OverdueDeparture Cards.overdueDeparture Nothing

instance HasAbilities OverdueDeparture where
  getAbilities (OverdueDeparture a) =
    [ restricted a 1 NoRestriction
        $ freeReaction
        $ oneOf
          [ EnemyEvaded #after You (EnemyAt campOutskirts)
          , EnemyDefeated #after You ByAny (EnemyAt campOutskirts)
          ]
    , onlyOnce
        $ restricted
          a
          2
          ( EachUndefeatedInvestigator (at_ campOutskirts)
              <> notExists (campOutskirts <> LocationWithAnyClues)
          )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage OverdueDeparture where
  runMessage msg a@(OverdueDeparture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach campOutskirts $ discoverAt NotInvestigate iid (attrs.ability 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> OverdueDeparture <$> liftRunMessage msg attrs
