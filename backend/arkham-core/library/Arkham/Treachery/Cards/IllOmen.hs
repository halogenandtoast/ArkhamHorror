module Arkham.Treachery.Cards.IllOmen (
  illOmen,
  IllOmen (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype IllOmen = IllOmen TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illOmen :: TreacheryCard IllOmen
illOmen = treachery IllOmen Cards.illOmen

instance RunMessage IllOmen where
  runMessage msg t@(IllOmen attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lids <- selectList $ LocationWithInvestigator UneliminatedInvestigator
      locationsWithInvestigators <- forToSnd lids (selectList . investigatorAt)
      push
        $ chooseOne
          iid
          [ targetLabel
            lid
            ( PlaceDoom (toSource attrs) (toTarget lid) 1
                : map
                  ( \i ->
                      InvestigatorAssignDamage i (toSource attrs) DamageAny 0 1
                  )
                  investigators
            )
          | (lid, investigators) <- locationsWithInvestigators
          ]
      pure t
    _ -> IllOmen <$> runMessage msg attrs
