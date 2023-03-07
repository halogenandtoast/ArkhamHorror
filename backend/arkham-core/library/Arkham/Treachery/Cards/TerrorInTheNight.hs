module Arkham.Treachery.Cards.TerrorInTheNight
  ( terrorInTheNight
  , TerrorInTheNight(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata { gainedSurge :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TerrorInTheNight = TerrorInTheNight (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorInTheNight :: TreacheryCard TerrorInTheNight
terrorInTheNight =
  treachery (TerrorInTheNight . (`with` Metadata False)) Cards.terrorInTheNight

instance HasModifiersFor TerrorInTheNight where
  getModifiersFor target (TerrorInTheNight (a `With` meta))
    | isTarget a target = pure
    $ toModifiers a [ AddKeyword Keyword.Surge | gainedSurge meta ]
  getModifiersFor _ _ = pure []

instance RunMessage TerrorInTheNight where
  runMessage msg t@(TerrorInTheNight (attrs `With` meta)) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower 4
      pure t
    FailedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ n
      -> do
        aid <- selectJust AnyAgenda
        other <- selectList (treacheryIs Cards.terrorInTheNight)
        iids <- getInvestigatorIds
        attached <- filterM
          (fieldMap
            TreacheryPlacement
            (== TreacheryAttachedTo (AgendaTarget aid))
          )
          other
        pushAll
          $ AttachTreachery (toId attrs) (AgendaTarget aid)
          : (guard (length attached >= 2)
            *> (Discard (toSource attrs) (toTarget attrs)
               : map (Discard (toSource attrs) . TreacheryTarget) attached
               <> [ InvestigatorAssignDamage
                      iid
                      (toSource attrs)
                      DamageAny
                      0
                      3
                  | iid <- iids
                  ]
               )
            )
        pure . TerrorInTheNight $ attrs `with` Metadata (n >= 3)
    _ -> TerrorInTheNight . (`with` meta) <$> runMessage msg attrs
