from Bio.Blast import NCBIXML
from Bio import Entrez, SeqIO
import csv

class Misc(object):

    # fasta_output = output name to save fasta sequences; gb_output = genbank output filename to save genbank records
    def __init__(self, fasta_output, gb_output, source_metadata_filename):
        self.fasta_output = fasta_output # Example: "/Users/user/Box Sync/HIV_myDesktop/data/B/HIV.B.source.fasta"
        self.gb_output = gb_output # Example: "/Users/user/Box Sync/HIV_myDesktop/data/B/HIV_subtypeB.gb"
        self.source_metadata_filename = source_metadata_filename

    def create_dict_from_variables(self, key, value):

        variable_dict = {key: value}
        return variable_dict

    def check_blast_records(self, results):

        count = 0

        # create list of Gi values
        gi_list = []

        # dictionary to create pairs of sequences
        # key will be GenBank query sequences and value will be GI for best hit
        dict = {}

        blast_records = NCBIXML.parse(results)

        E_VALUE_THRESH = 1e-3
        for blast_record in blast_records:

            query = str(blast_record.query)
            query = query.split(".")
            print(query[0])
            # print ('query name', blast_record.query)
            for alignment in blast_record.alignments:
                for hsp in alignment.hsps:

                    if hsp.expect < E_VALUE_THRESH:

                        if count < 1:

                            # print('****Alignment****')
                            # print('sequence:', str(alignment.hit_id))
                            seq = str(alignment.hit_id)
                            seq = seq.split("|")
                            # print(seq)
                            GenBank = seq[3].split(".")
                            # print(GenBank[0])

                            if GenBank[0] != query[0]:

                                # print("Genbank: ", GenBank[0])
                                # print("GI: ", seq[1])

                                update_dict = self.create_dict_from_variables(query[0], seq[1])

                                dict.update(update_dict)

                                if seq[1] not in gi_list:
                                    gi_list.append(seq[1])

                                count = count + 1
                                # print (count)

            count = 0  # reset count to zero for the next blast query

        # convert gi list to a more appropriate version to be used Entrez
        gi_str = ",".join(gi_list)

        return gi_str

    def download_genbank(self, gis):
        Entrez.email = "f.nascimento@imperial.ac.uk" # CHANGE to your e-mail address here!!
        handle = Entrez.efetch(db="nuccore", id=gis, rettype="gb", retmode="text")
        records = SeqIO.parse(handle, "gb")

        # save GenBak records to file

        # Save GenBank records to a file called HIV_subtypeB.gb
        output_file = open(self.gb_output, 'w')
        SeqIO.write(records, output_file, "genbank")

    # get gi for entries in which country is NOT Senegal
    # it will also save as fasta the genbank entries for the source sequences
    def get_source_genbank(self, gb_records):
        key = "country"

        # list of GenBank accession numbers that does not match "Senegal" as country
        # This will be the list containing the source GenBank accession numbers
        accessions_list = []
        country_list = []
        year_list = []

        fasta_out = self.fasta_output
        output_file = open(fasta_out, "a")

        for record in gb_records:
            for f in record.features:

                if f.qualifiers.get(key) != None:

                    country = f.qualifiers.get(key)
                    print("country: ", country[0])
                    country = country[0].lower()

                    if "senegal" not in country:
                        # print(record.name)
                        accessions_list.append(record.name)
                        country_list.append(country)
                        if f.qualifiers.get("collection_date") != None:
                            year_list.append(f.qualifiers.get("collection_date")[0])
                        else:
                            year_list.append("N/A")
                        # print(accessions_list)

                        SeqIO.write(record, output_file, "fasta")

        output_file.close()
        self.save_source_metadata(self.source_metadata_filename, accessions_list, country_list, year_list)

        return accessions_list


    #filename should include filename to save the metadata and path to save it
    # as for exampple: "/Users/user/Desktop/SOURCE_TESTE.csv"
    def save_source_metadata(self, filename, genbank_list, country_list, year_list):

        with open(filename, 'w') as csvfile:
            writer = csv.writer(csvfile, delimiter=',')
            # write header to csv file
            writer.writerow(("GenBank", "Country", "Collection_year"))

            for genbank, country, year in zip(genbank_list, country_list, year_list):
                writer.writerow([genbank, country, year])
