module Arkham.Types.Treachery.Cards.UmordhothsWrath where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype UmordhothsWrath = UmordhothsWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsWrath :: TreacheryCard UmordhothsWrath
umordhothsWrath = treachery UmordhothsWrath Cards.umordhothsWrath

instance TreacheryRunner env => RunMessage env UmordhothsWrath where
  runMessage msg t@(UmordhothsWrath attrs@TreacheryAttrs {..}) = case msg of
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t
      <$ push (HandlePointOfFailure iid (TreacheryTarget tid) n)
    HandlePointOfFailure _ (TreacheryTarget tid) 0 | tid == treacheryId ->
      pure t
    HandlePointOfFailure iid (TreacheryTarget tid) n | tid == treacheryId -> do
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' > 0
        then t <$ pushAll
          [ chooseOne
            iid
            [ Label "Discard a card from your hand" [RandomDiscard iid]
            , Label
              "Take 1 damage and 1 horror"
              [ InvestigatorAssignDamage
                  iid
                  (TreacherySource treacheryId)
                  DamageAny
                  1
                  1
              ]
            ]
          , HandlePointOfFailure iid (TreacheryTarget treacheryId) (n - 1)
          ]
        else t <$ pushAll
          [ InvestigatorAssignDamage
            iid
            (TreacherySource treacheryId)
            DamageAny
            1
            1
          , HandlePointOfFailure iid (TreacheryTarget treacheryId) (n - 1)
          ]
    Revelation iid source | isSource attrs source -> t <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        5
      )
    _ -> UmordhothsWrath <$> runMessage msg attrs
